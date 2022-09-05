/*
 *  Copyright (c) 2013 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */
#include "video/video_send_stream.h"

#include <utility>

#include "api/video/video_stream_encoder_create.h"
#include "modules/rtp_rtcp/source/rtp_header_extension_size.h"
#include "modules/rtp_rtcp/source/rtp_sender.h"
#include "rtc_base/logging.h"
#include "video/video_send_stream_impl.h"

namespace webrtc {

namespace {

size_t CalculateMaxHeaderSize(const RtpConfig& config) {
  size_t header_size = kRtpHeaderSize;
  size_t extensions_size = 0;
  size_t fec_extensions_size = 0;
  if (config.extensions.size() > 0) {
    RtpHeaderExtensionMap extensions_map(config.extensions);
    extensions_size = RtpHeaderExtensionSize(RTPSender::VideoExtensionSizes(),
                                             extensions_map);
    fec_extensions_size =
        RtpHeaderExtensionSize(RTPSender::FecExtensionSizes(), extensions_map);
  }
  header_size += extensions_size;
  if (config.flexfec.payload_type >= 0) {
    // All FEC extensions again plus maximum FlexFec overhead.
    header_size += fec_extensions_size + 32;
  } else {
    if (config.ulpfec.ulpfec_payload_type >= 0) {
      // Header with all the FEC extensions will be repeated plus maximum
      // UlpFec overhead.
      header_size += fec_extensions_size + 18;
    }
    if (config.ulpfec.red_payload_type >= 0) {
      header_size += 1;  // RED header.
    }
  }
  // Additional room for Rtx.
  if (config.rtx.payload_type >= 0)
    header_size += kRtxHeaderSize;
  return header_size;
}

}  // namespace

namespace internal {

VideoSendStream::VideoSendStream(
    int num_cpu_cores,
    ProcessThread* module_process_thread,
    rtc::TaskQueue* worker_queue,
    CallStats* call_stats,
    RtpTransportControllerSendInterface* transport,
    BitrateAllocatorInterface* bitrate_allocator,
    SendDelayStats* send_delay_stats,
    RtcEventLog* event_log,
    VideoSendStream::Config config,
    VideoEncoderConfig encoder_config,
    const std::map<uint32_t, RtpState>& suspended_ssrcs,
    const std::map<uint32_t, RtpPayloadState>& suspended_payload_states,
    std::unique_ptr<FecController> fec_controller)
    : worker_queue_(worker_queue),
      thread_sync_event_(false /* manual_reset */, false),
      stats_proxy_(Clock::GetRealTimeClock(),
                   config,
                   encoder_config.content_type),
      config_(std::move(config)),
      content_type_(encoder_config.content_type) {
  RTC_DCHECK(config_.encoder_settings.encoder_factory);

  video_stream_encoder_ = CreateVideoStreamEncoder(num_cpu_cores, &stats_proxy_,
                                                   config_.encoder_settings,
                                                   config_.pre_encode_callback);
  // TODO(srte): Initialization should not be done posted on a task queue.
  // Note that the posted task must not outlive this scope since the closure
  // references local variables.
  worker_queue_->PostTask(rtc::NewClosure(
      [this, call_stats, transport, bitrate_allocator, send_delay_stats,
       event_log, &suspended_ssrcs, &encoder_config, &suspended_payload_states,
       &fec_controller]() {
        send_stream_.reset(new VideoSendStreamImpl(
            &stats_proxy_, worker_queue_, call_stats, transport,
            bitrate_allocator, send_delay_stats, video_stream_encoder_.get(),
            event_log, &config_, encoder_config.max_bitrate_bps,
            encoder_config.bitrate_priority, suspended_ssrcs,
            suspended_payload_states, encoder_config.content_type,
            std::move(fec_controller)));
      },
      [this]() { thread_sync_event_.Set(); }));

  // Wait for ConstructionTask to complete so that |send_stream_| can be used.
  // |module_process_thread| must be registered and deregistered on the thread
  // it was created on.
  thread_sync_event_.Wait(rtc::Event::kForever);
  send_stream_->RegisterProcessThread(module_process_thread);
  // TODO(sprang): Enable this also for regular video calls if it works well.
  if (encoder_config.content_type == VideoEncoderConfig::ContentType::kScreen) {
    // Only signal target bitrate for screenshare streams, for now.
    video_stream_encoder_->SetBitrateAllocationObserver(send_stream_.get());
  }

  ReconfigureVideoEncoder(std::move(encoder_config));
}

VideoSendStream::~VideoSendStream() {
  RTC_DCHECK_RUN_ON(&thread_checker_);
  RTC_DCHECK(!send_stream_);
}

void VideoSendStream::UpdateActiveSimulcastLayers(
    const std::vector<bool> active_layers) {
  RTC_DCHECK_RUN_ON(&thread_checker_);
  RTC_LOG(LS_INFO) << "VideoSendStream::UpdateActiveSimulcastLayers";
  VideoSendStreamImpl* send_stream = send_stream_.get();
  worker_queue_->PostTask([this, send_stream, active_layers] {
    send_stream->UpdateActiveSimulcastLayers(active_layers);
    thread_sync_event_.Set();
  });

  thread_sync_event_.Wait(rtc::Event::kForever);
}

void VideoSendStream::Start() {
  RTC_DCHECK_RUN_ON(&thread_checker_);
  RTC_LOG(LS_INFO) << "VideoSendStream::Start";
  VideoSendStreamImpl* send_stream = send_stream_.get();
  worker_queue_->PostTask([this, send_stream] {
    send_stream->Start();
    thread_sync_event_.Set();
  });

  // It is expected that after VideoSendStream::Start has been called, incoming
  // frames are not dropped in VideoStreamEncoder. To ensure this, Start has to
  // be synchronized.
  thread_sync_event_.Wait(rtc::Event::kForever);
}

void VideoSendStream::Stop() {
  RTC_DCHECK_RUN_ON(&thread_checker_);
  RTC_LOG(LS_INFO) << "VideoSendStream::Stop";
  VideoSendStreamImpl* send_stream = send_stream_.get();
  worker_queue_->PostTask([send_stream] { send_stream->Stop(); });
}

void VideoSendStream::SetSource(
    rtc::VideoSourceInterface<webrtc::VideoFrame>* source,
    const DegradationPreference& degradation_preference) {
  RTC_DCHECK_RUN_ON(&thread_checker_);
  video_stream_encoder_->SetSource(source, degradation_preference);
}

void VideoSendStream::ReconfigureVideoEncoder(VideoEncoderConfig config) {
  // TODO(perkj): Some test cases in VideoSendStreamTest call
  // ReconfigureVideoEncoder from the network thread.
  // RTC_DCHECK_RUN_ON(&thread_checker_);
  RTC_DCHECK(content_type_ == config.content_type);
  video_stream_encoder_->ConfigureEncoder(
      std::move(config),
      config_.rtp.max_packet_size - CalculateMaxHeaderSize(config_.rtp));
}

VideoSendStream::Stats VideoSendStream::GetStats() {
  // TODO(perkj, solenberg): Some test cases in EndToEndTest call GetStats from
  // a network thread. See comment in Call::GetStats().
  // RTC_DCHECK_RUN_ON(&thread_checker_);
  return stats_proxy_.GetStats();
}

absl::optional<float> VideoSendStream::GetPacingFactorOverride() const {
  return send_stream_->configured_pacing_factor_;
}

void VideoSendStream::StopPermanentlyAndGetRtpStates(
    VideoSendStream::RtpStateMap* rtp_state_map,
    VideoSendStream::RtpPayloadStateMap* payload_state_map) {
  RTC_DCHECK_RUN_ON(&thread_checker_);
  video_stream_encoder_->Stop();
  send_stream_->DeRegisterProcessThread();
  worker_queue_->PostTask([this, rtp_state_map, payload_state_map]() {
    send_stream_->Stop();
    *rtp_state_map = send_stream_->GetRtpStates();
    *payload_state_map = send_stream_->GetRtpPayloadStates();
    send_stream_.reset();
    thread_sync_event_.Set();
  });
  thread_sync_event_.Wait(rtc::Event::kForever);
}

bool VideoSendStream::DeliverRtcp(const uint8_t* packet, size_t length) {
  // Called on a network thread.
  return send_stream_->DeliverRtcp(packet, length);
}

}  // namespace internal
}  // namespace webrtc
