/*
 *  Copyright 2018 The WebRTC Project Authors. All rights reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */

#ifndef API_TEST_LOOPBACK_MEDIA_TRANSPORT_H_
#define API_TEST_LOOPBACK_MEDIA_TRANSPORT_H_

#include <utility>

#include "api/media_transport_interface.h"

namespace webrtc {

// Contains two MediaTransportsInterfaces that are connected to each other.
// Currently supports audio only.
class MediaTransportPair {
 public:
  MediaTransportPair()
      : pipe_{LoopbackMediaTransport(&pipe_[1]),
              LoopbackMediaTransport(&pipe_[0])} {}

  // Ownership stays with MediaTransportPair
  MediaTransportInterface* first() { return &pipe_[0]; }
  MediaTransportInterface* second() { return &pipe_[1]; }

 private:
  class LoopbackMediaTransport : public MediaTransportInterface {
   public:
    explicit LoopbackMediaTransport(LoopbackMediaTransport* other)
        : other_(other) {}
    ~LoopbackMediaTransport() { RTC_CHECK(sink_ == nullptr); }

    RTCError SendAudioFrame(uint64_t channel_id,
                            MediaTransportEncodedAudioFrame frame) override {
      other_->OnData(channel_id, std::move(frame));
      return RTCError::OK();
    };

    RTCError SendVideoFrame(
        uint64_t channel_id,
        const MediaTransportEncodedVideoFrame& frame) override {
      return RTCError::OK();
    }

    RTCError RequestKeyFrame(uint64_t channel_id) override {
      return RTCError::OK();
    }

    void SetReceiveAudioSink(MediaTransportAudioSinkInterface* sink) override {
      if (sink) {
        RTC_CHECK(sink_ == nullptr);
      }
      sink_ = sink;
    }

    void SetReceiveVideoSink(MediaTransportVideoSinkInterface* sink) override {}

   private:
    void OnData(uint64_t channel_id, MediaTransportEncodedAudioFrame frame) {
      if (sink_) {
        sink_->OnData(channel_id, frame);
      }
    }

    MediaTransportAudioSinkInterface* sink_ = nullptr;
    LoopbackMediaTransport* other_;
  };

  LoopbackMediaTransport pipe_[2];
};

}  // namespace webrtc

#endif  // API_TEST_LOOPBACK_MEDIA_TRANSPORT_H_
