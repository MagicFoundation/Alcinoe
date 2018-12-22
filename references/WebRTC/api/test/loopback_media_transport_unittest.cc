/*
 *  Copyright 2018 The WebRTC Project Authors. All rights reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */

#include <vector>

#include "api/test/loopback_media_transport.h"
#include "test/gmock.h"

namespace webrtc {

namespace {

class MockMediaTransportAudioSinkInterface
    : public MediaTransportAudioSinkInterface {
 public:
  MOCK_METHOD2(OnData, void(uint64_t, MediaTransportEncodedAudioFrame));
};

// Test only uses the sequence number.
MediaTransportEncodedAudioFrame CreateAudioFrame(int sequence_number) {
  static constexpr int kSamplingRateHz = 48000;
  static constexpr int kStartingSampleIndex = 0;
  static constexpr int kSamplesPerChannel = 480;
  static constexpr uint8_t kPayloadType = 17;

  return MediaTransportEncodedAudioFrame(
      kSamplingRateHz, kStartingSampleIndex, kSamplesPerChannel,
      sequence_number, MediaTransportEncodedAudioFrame::FrameType::kSpeech,
      kPayloadType, std::vector<uint8_t>(kSamplesPerChannel));
}

}  // namespace

TEST(LoopbackMediaTransport, AudioWithNoSinkSilentlyIgnored) {
  MediaTransportPair transport_pair;
  transport_pair.first()->SendAudioFrame(1, CreateAudioFrame(0));
  transport_pair.second()->SendAudioFrame(2, CreateAudioFrame(0));
}

TEST(LoopbackMediaTransport, AudioDeliveredToSink) {
  MediaTransportPair transport_pair;
  testing::StrictMock<MockMediaTransportAudioSinkInterface> sink;
  EXPECT_CALL(sink,
              OnData(1, testing::Property(
                            &MediaTransportEncodedAudioFrame::sequence_number,
                            testing::Eq(10))));
  transport_pair.second()->SetReceiveAudioSink(&sink);
  transport_pair.first()->SendAudioFrame(1, CreateAudioFrame(10));

  transport_pair.second()->SetReceiveAudioSink(nullptr);
}

}  // namespace webrtc
