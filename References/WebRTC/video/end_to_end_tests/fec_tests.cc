/*
 *  Copyright 2018 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */

#include "api/test/simulated_network.h"
#include "call/fake_network_pipe.h"
#include "call/simulated_network.h"
#include "media/engine/internaldecoderfactory.h"
#include "modules/rtp_rtcp/source/byte_io.h"
#include "modules/video_coding/codecs/vp8/include/vp8.h"
#include "test/call_test.h"
#include "test/field_trial.h"
#include "test/function_video_encoder_factory.h"
#include "test/gtest.h"
#include "test/rtcp_packet_parser.h"

namespace webrtc {

class FecEndToEndTest : public test::CallTest {
 public:
  FecEndToEndTest() = default;
};

TEST_F(FecEndToEndTest, ReceivesUlpfec) {
  class UlpfecRenderObserver : public test::EndToEndTest,
                               public rtc::VideoSinkInterface<VideoFrame> {
   public:
    UlpfecRenderObserver()
        : EndToEndTest(kDefaultTimeoutMs),
          encoder_factory_([]() { return VP8Encoder::Create(); }),
          random_(0xcafef00d1),
          num_packets_sent_(0) {}

   private:
    Action OnSendRtp(const uint8_t* packet, size_t length) override {
      rtc::CritScope lock(&crit_);
      RTPHeader header;
      EXPECT_TRUE(parser_->Parse(packet, length, &header));

      EXPECT_TRUE(header.payloadType == kVideoSendPayloadType ||
                  header.payloadType == kRedPayloadType)
          << "Unknown payload type received.";
      EXPECT_EQ(kVideoSendSsrcs[0], header.ssrc) << "Unknown SSRC received.";

      // Parse RED header.
      int encapsulated_payload_type = -1;
      if (header.payloadType == kRedPayloadType) {
        encapsulated_payload_type =
            static_cast<int>(packet[header.headerLength]);

        EXPECT_TRUE(encapsulated_payload_type == kVideoSendPayloadType ||
                    encapsulated_payload_type == kUlpfecPayloadType)
            << "Unknown encapsulated payload type received.";
      }

      // To minimize test flakiness, always let ULPFEC packets through.
      if (encapsulated_payload_type == kUlpfecPayloadType) {
        return SEND_PACKET;
      }

      // Simulate 5% video packet loss after rampup period. Record the
      // corresponding timestamps that were dropped.
      if (num_packets_sent_++ > 100 && random_.Rand(1, 100) <= 5) {
        if (encapsulated_payload_type == kVideoSendPayloadType) {
          dropped_sequence_numbers_.insert(header.sequenceNumber);
          dropped_timestamps_.insert(header.timestamp);
        }
        return DROP_PACKET;
      }

      return SEND_PACKET;
    }

    void OnFrame(const VideoFrame& video_frame) override {
      rtc::CritScope lock(&crit_);
      // Rendering frame with timestamp of packet that was dropped -> FEC
      // protection worked.
      auto it = dropped_timestamps_.find(video_frame.timestamp());
      if (it != dropped_timestamps_.end()) {
        observation_complete_.Set();
      }
    }

    void ModifyVideoConfigs(
        VideoSendStream::Config* send_config,
        std::vector<VideoReceiveStream::Config>* receive_configs,
        VideoEncoderConfig* encoder_config) override {
      // Use VP8 instead of FAKE, since the latter does not have PictureID
      // in the packetization headers.
      send_config->encoder_settings.encoder_factory = &encoder_factory_;
      send_config->rtp.payload_name = "VP8";
      send_config->rtp.payload_type = kVideoSendPayloadType;
      encoder_config->codec_type = kVideoCodecVP8;
      VideoReceiveStream::Decoder decoder =
          test::CreateMatchingDecoder(*send_config);
      decoder.decoder_factory = &decoder_factory_;
      (*receive_configs)[0].decoders.clear();
      (*receive_configs)[0].decoders.push_back(decoder);

      // Enable ULPFEC over RED.
      send_config->rtp.ulpfec.red_payload_type = kRedPayloadType;
      send_config->rtp.ulpfec.ulpfec_payload_type = kUlpfecPayloadType;
      (*receive_configs)[0].rtp.red_payload_type = kRedPayloadType;
      (*receive_configs)[0].rtp.ulpfec_payload_type = kUlpfecPayloadType;

      (*receive_configs)[0].renderer = this;
    }

    void PerformTest() override {
      EXPECT_TRUE(Wait())
          << "Timed out waiting for dropped frames to be rendered.";
    }

    rtc::CriticalSection crit_;
    std::unique_ptr<VideoEncoder> encoder_;
    test::FunctionVideoEncoderFactory encoder_factory_;
    InternalDecoderFactory decoder_factory_;
    std::set<uint32_t> dropped_sequence_numbers_ RTC_GUARDED_BY(crit_);
    // Several packets can have the same timestamp.
    std::multiset<uint32_t> dropped_timestamps_ RTC_GUARDED_BY(crit_);
    Random random_;
    int num_packets_sent_ RTC_GUARDED_BY(crit_);
  } test;

  RunBaseTest(&test);
}

class FlexfecRenderObserver : public test::EndToEndTest,
                              public rtc::VideoSinkInterface<VideoFrame> {
 public:
  static constexpr uint32_t kVideoLocalSsrc = 123;
  static constexpr uint32_t kFlexfecLocalSsrc = 456;

  explicit FlexfecRenderObserver(bool enable_nack, bool expect_flexfec_rtcp)
      : test::EndToEndTest(test::CallTest::kDefaultTimeoutMs),
        enable_nack_(enable_nack),
        expect_flexfec_rtcp_(expect_flexfec_rtcp),
        received_flexfec_rtcp_(false),
        random_(0xcafef00d1),
        num_packets_sent_(0) {}

  size_t GetNumFlexfecStreams() const override { return 1; }

 private:
  Action OnSendRtp(const uint8_t* packet, size_t length) override {
    rtc::CritScope lock(&crit_);
    RTPHeader header;
    EXPECT_TRUE(parser_->Parse(packet, length, &header));

    EXPECT_TRUE(header.payloadType ==
                    test::CallTest::kFakeVideoSendPayloadType ||
                header.payloadType == test::CallTest::kFlexfecPayloadType ||
                (enable_nack_ &&
                 header.payloadType == test::CallTest::kSendRtxPayloadType))
        << "Unknown payload type received.";
    EXPECT_TRUE(
        header.ssrc == test::CallTest::kVideoSendSsrcs[0] ||
        header.ssrc == test::CallTest::kFlexfecSendSsrc ||
        (enable_nack_ && header.ssrc == test::CallTest::kSendRtxSsrcs[0]))
        << "Unknown SSRC received.";

    // To reduce test flakiness, always let FlexFEC packets through.
    if (header.payloadType == test::CallTest::kFlexfecPayloadType) {
      EXPECT_EQ(test::CallTest::kFlexfecSendSsrc, header.ssrc);

      return SEND_PACKET;
    }

    // To reduce test flakiness, always let RTX packets through.
    if (header.payloadType == test::CallTest::kSendRtxPayloadType) {
      EXPECT_EQ(test::CallTest::kSendRtxSsrcs[0], header.ssrc);

      // Parse RTX header.
      uint16_t original_sequence_number =
          ByteReader<uint16_t>::ReadBigEndian(&packet[header.headerLength]);

      // From the perspective of FEC, a retransmitted packet is no longer
      // dropped, so remove it from list of dropped packets.
      auto seq_num_it =
          dropped_sequence_numbers_.find(original_sequence_number);
      if (seq_num_it != dropped_sequence_numbers_.end()) {
        dropped_sequence_numbers_.erase(seq_num_it);
        auto ts_it = dropped_timestamps_.find(header.timestamp);
        EXPECT_NE(ts_it, dropped_timestamps_.end());
        dropped_timestamps_.erase(ts_it);
      }

      return SEND_PACKET;
    }

    // Simulate 5% video packet loss after rampup period. Record the
    // corresponding timestamps that were dropped.
    if (num_packets_sent_++ > 100 && random_.Rand(1, 100) <= 5) {
      EXPECT_EQ(test::CallTest::kFakeVideoSendPayloadType, header.payloadType);
      EXPECT_EQ(test::CallTest::kVideoSendSsrcs[0], header.ssrc);

      dropped_sequence_numbers_.insert(header.sequenceNumber);
      dropped_timestamps_.insert(header.timestamp);

      return DROP_PACKET;
    }

    return SEND_PACKET;
  }

  Action OnReceiveRtcp(const uint8_t* data, size_t length) override {
    test::RtcpPacketParser parser;

    parser.Parse(data, length);
    if (parser.sender_ssrc() == kFlexfecLocalSsrc) {
      EXPECT_EQ(1, parser.receiver_report()->num_packets());
      const std::vector<rtcp::ReportBlock>& report_blocks =
          parser.receiver_report()->report_blocks();
      if (!report_blocks.empty()) {
        EXPECT_EQ(1U, report_blocks.size());
        EXPECT_EQ(test::CallTest::kFlexfecSendSsrc,
                  report_blocks[0].source_ssrc());
        rtc::CritScope lock(&crit_);
        received_flexfec_rtcp_ = true;
      }
    }

    return SEND_PACKET;
  }

  test::PacketTransport* CreateSendTransport(
      test::SingleThreadedTaskQueueForTesting* task_queue,
      Call* sender_call) override {
    // At low RTT (< kLowRttNackMs) -> NACK only, no FEC.
    const int kNetworkDelayMs = 100;
    BuiltInNetworkBehaviorConfig config;
    config.queue_delay_ms = kNetworkDelayMs;
    return new test::PacketTransport(
        task_queue, sender_call, this, test::PacketTransport::kSender,
        test::CallTest::payload_type_map_,
        absl::make_unique<FakeNetworkPipe>(
            Clock::GetRealTimeClock(),
            absl::make_unique<SimulatedNetwork>(config)));
  }

  void OnFrame(const VideoFrame& video_frame) override {
    EXPECT_EQ(kVideoRotation_90, video_frame.rotation());

    rtc::CritScope lock(&crit_);
    // Rendering frame with timestamp of packet that was dropped -> FEC
    // protection worked.
    auto it = dropped_timestamps_.find(video_frame.timestamp());
    if (it != dropped_timestamps_.end()) {
      if (!expect_flexfec_rtcp_ || received_flexfec_rtcp_) {
        observation_complete_.Set();
      }
    }
  }

  void ModifyVideoConfigs(
      VideoSendStream::Config* send_config,
      std::vector<VideoReceiveStream::Config>* receive_configs,
      VideoEncoderConfig* encoder_config) override {
    (*receive_configs)[0].rtp.local_ssrc = kVideoLocalSsrc;
    (*receive_configs)[0].renderer = this;

    if (enable_nack_) {
      send_config->rtp.nack.rtp_history_ms = test::CallTest::kNackRtpHistoryMs;
      send_config->rtp.rtx.ssrcs.push_back(test::CallTest::kSendRtxSsrcs[0]);
      send_config->rtp.rtx.payload_type = test::CallTest::kSendRtxPayloadType;

      (*receive_configs)[0].rtp.nack.rtp_history_ms =
          test::CallTest::kNackRtpHistoryMs;
      (*receive_configs)[0].rtp.rtx_ssrc = test::CallTest::kSendRtxSsrcs[0];
      (*receive_configs)[0]
          .rtp
          .rtx_associated_payload_types[test::CallTest::kSendRtxPayloadType] =
          test::CallTest::kVideoSendPayloadType;
    }
  }

  void OnFrameGeneratorCapturerCreated(
      test::FrameGeneratorCapturer* frame_generator_capturer) override {
    frame_generator_capturer->SetFakeRotation(kVideoRotation_90);
  }

  void ModifyFlexfecConfigs(
      std::vector<FlexfecReceiveStream::Config>* receive_configs) override {
    (*receive_configs)[0].local_ssrc = kFlexfecLocalSsrc;
  }

  void PerformTest() override {
    EXPECT_TRUE(Wait())
        << "Timed out waiting for dropped frames to be rendered.";
  }

  rtc::CriticalSection crit_;
  std::set<uint32_t> dropped_sequence_numbers_ RTC_GUARDED_BY(crit_);
  // Several packets can have the same timestamp.
  std::multiset<uint32_t> dropped_timestamps_ RTC_GUARDED_BY(crit_);
  const bool enable_nack_;
  const bool expect_flexfec_rtcp_;
  bool received_flexfec_rtcp_ RTC_GUARDED_BY(crit_);
  Random random_;
  int num_packets_sent_;
};

TEST_F(FecEndToEndTest, RecoversWithFlexfec) {
  FlexfecRenderObserver test(false, false);
  RunBaseTest(&test);
}

TEST_F(FecEndToEndTest, RecoversWithFlexfecAndNack) {
  FlexfecRenderObserver test(true, false);
  RunBaseTest(&test);
}

TEST_F(FecEndToEndTest, RecoversWithFlexfecAndSendsCorrespondingRtcp) {
  FlexfecRenderObserver test(false, true);
  RunBaseTest(&test);
}

TEST_F(FecEndToEndTest, ReceivedUlpfecPacketsNotNacked) {
  class UlpfecNackObserver : public test::EndToEndTest {
   public:
    UlpfecNackObserver()
        : EndToEndTest(kDefaultTimeoutMs),
          state_(kFirstPacket),
          ulpfec_sequence_number_(0),
          has_last_sequence_number_(false),
          last_sequence_number_(0),
          encoder_factory_([]() { return VP8Encoder::Create(); }) {}

   private:
    Action OnSendRtp(const uint8_t* packet, size_t length) override {
      rtc::CritScope lock_(&crit_);
      RTPHeader header;
      EXPECT_TRUE(parser_->Parse(packet, length, &header));

      int encapsulated_payload_type = -1;
      if (header.payloadType == kRedPayloadType) {
        encapsulated_payload_type =
            static_cast<int>(packet[header.headerLength]);
        if (encapsulated_payload_type != kFakeVideoSendPayloadType)
          EXPECT_EQ(kUlpfecPayloadType, encapsulated_payload_type);
      } else {
        EXPECT_EQ(kFakeVideoSendPayloadType, header.payloadType);
      }

      if (has_last_sequence_number_ &&
          !IsNewerSequenceNumber(header.sequenceNumber,
                                 last_sequence_number_)) {
        // Drop retransmitted packets.
        return DROP_PACKET;
      }
      last_sequence_number_ = header.sequenceNumber;
      has_last_sequence_number_ = true;

      bool ulpfec_packet = encapsulated_payload_type == kUlpfecPayloadType;
      switch (state_) {
        case kFirstPacket:
          state_ = kDropEveryOtherPacketUntilUlpfec;
          break;
        case kDropEveryOtherPacketUntilUlpfec:
          if (ulpfec_packet) {
            state_ = kDropAllMediaPacketsUntilUlpfec;
          } else if (header.sequenceNumber % 2 == 0) {
            return DROP_PACKET;
          }
          break;
        case kDropAllMediaPacketsUntilUlpfec:
          if (!ulpfec_packet)
            return DROP_PACKET;
          ulpfec_sequence_number_ = header.sequenceNumber;
          state_ = kDropOneMediaPacket;
          break;
        case kDropOneMediaPacket:
          if (ulpfec_packet)
            return DROP_PACKET;
          state_ = kPassOneMediaPacket;
          return DROP_PACKET;
          break;
        case kPassOneMediaPacket:
          if (ulpfec_packet)
            return DROP_PACKET;
          // Pass one media packet after dropped packet after last FEC,
          // otherwise receiver might never see a seq_no after
          // |ulpfec_sequence_number_|
          state_ = kVerifyUlpfecPacketNotInNackList;
          break;
        case kVerifyUlpfecPacketNotInNackList:
          // Continue to drop packets. Make sure no frame can be decoded.
          if (ulpfec_packet || header.sequenceNumber % 2 == 0)
            return DROP_PACKET;
          break;
      }
      return SEND_PACKET;
    }

    Action OnReceiveRtcp(const uint8_t* packet, size_t length) override {
      rtc::CritScope lock_(&crit_);
      if (state_ == kVerifyUlpfecPacketNotInNackList) {
        test::RtcpPacketParser rtcp_parser;
        rtcp_parser.Parse(packet, length);
        const std::vector<uint16_t>& nacks = rtcp_parser.nack()->packet_ids();
        EXPECT_TRUE(std::find(nacks.begin(), nacks.end(),
                              ulpfec_sequence_number_) == nacks.end())
            << "Got nack for ULPFEC packet";
        if (!nacks.empty() &&
            IsNewerSequenceNumber(nacks.back(), ulpfec_sequence_number_)) {
          observation_complete_.Set();
        }
      }
      return SEND_PACKET;
    }

    test::PacketTransport* CreateSendTransport(
        test::SingleThreadedTaskQueueForTesting* task_queue,
        Call* sender_call) override {
      // At low RTT (< kLowRttNackMs) -> NACK only, no FEC.
      // Configure some network delay.
      const int kNetworkDelayMs = 50;
      BuiltInNetworkBehaviorConfig config;
      config.queue_delay_ms = kNetworkDelayMs;
      return new test::PacketTransport(
          task_queue, sender_call, this, test::PacketTransport::kSender,
          payload_type_map_,
          absl::make_unique<FakeNetworkPipe>(
              Clock::GetRealTimeClock(),
              absl::make_unique<SimulatedNetwork>(config)));
    }

    // TODO(holmer): Investigate why we don't send FEC packets when the bitrate
    // is 10 kbps.
    void ModifySenderCallConfig(Call::Config* config) override {
      const int kMinBitrateBps = 30000;
      config->bitrate_config.min_bitrate_bps = kMinBitrateBps;
    }

    void ModifyVideoConfigs(
        VideoSendStream::Config* send_config,
        std::vector<VideoReceiveStream::Config>* receive_configs,
        VideoEncoderConfig* encoder_config) override {
      // Configure hybrid NACK/FEC.
      send_config->rtp.nack.rtp_history_ms = kNackRtpHistoryMs;
      send_config->rtp.ulpfec.red_payload_type = kRedPayloadType;
      send_config->rtp.ulpfec.ulpfec_payload_type = kUlpfecPayloadType;
      // Set codec to VP8, otherwise NACK/FEC hybrid will be disabled.
      send_config->encoder_settings.encoder_factory = &encoder_factory_;
      send_config->rtp.payload_name = "VP8";
      send_config->rtp.payload_type = kFakeVideoSendPayloadType;
      encoder_config->codec_type = kVideoCodecVP8;

      (*receive_configs)[0].rtp.nack.rtp_history_ms = kNackRtpHistoryMs;
      (*receive_configs)[0].rtp.red_payload_type = kRedPayloadType;
      (*receive_configs)[0].rtp.ulpfec_payload_type = kUlpfecPayloadType;

      (*receive_configs)[0].decoders.resize(1);
      (*receive_configs)[0].decoders[0].payload_type =
          send_config->rtp.payload_type;
      (*receive_configs)[0].decoders[0].video_format =
          SdpVideoFormat(send_config->rtp.payload_name);
      (*receive_configs)[0].decoders[0].decoder_factory = &decoder_factory_;
    }

    void PerformTest() override {
      EXPECT_TRUE(Wait())
          << "Timed out while waiting for FEC packets to be received.";
    }

    enum {
      kFirstPacket,
      kDropEveryOtherPacketUntilUlpfec,
      kDropAllMediaPacketsUntilUlpfec,
      kDropOneMediaPacket,
      kPassOneMediaPacket,
      kVerifyUlpfecPacketNotInNackList,
    } state_;

    rtc::CriticalSection crit_;
    uint16_t ulpfec_sequence_number_ RTC_GUARDED_BY(&crit_);
    bool has_last_sequence_number_;
    uint16_t last_sequence_number_;
    test::FunctionVideoEncoderFactory encoder_factory_;
    InternalDecoderFactory decoder_factory_;
  } test;

  RunBaseTest(&test);
}
}  // namespace webrtc
