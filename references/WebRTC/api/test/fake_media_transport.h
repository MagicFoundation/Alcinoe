/*
 *  Copyright 2018 The WebRTC Project Authors. All rights reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */

#ifndef API_TEST_FAKE_MEDIA_TRANSPORT_H_
#define API_TEST_FAKE_MEDIA_TRANSPORT_H_

#include <memory>
#include <utility>

#include "absl/memory/memory.h"
#include "api/media_transport_interface.h"

namespace webrtc {

// TODO(sukhanov): For now fake media transport does nothing and is used only
// in jsepcontroller unittests. In the future we should implement fake media
// transport, which forwards frames to another fake media transport, so we
// could unit test audio / video integration.
class FakeMediaTransport : public MediaTransportInterface {
 public:
  explicit FakeMediaTransport(bool is_caller) : is_caller_(is_caller) {}
  ~FakeMediaTransport() = default;

  RTCError SendAudioFrame(uint64_t channel_id,
                          MediaTransportEncodedAudioFrame frame) override {
    return RTCError::OK();
  }

  RTCError SendVideoFrame(
      uint64_t channel_id,
      const MediaTransportEncodedVideoFrame& frame) override {
    return RTCError::OK();
  }

  RTCError RequestKeyFrame(uint64_t channel_id) override {
    return RTCError::OK();
  };

  void SetReceiveAudioSink(MediaTransportAudioSinkInterface* sink) override {}
  void SetReceiveVideoSink(MediaTransportVideoSinkInterface* sink) override {}

  // Returns true if fake media trasport was created as a caller.
  bool is_caller() const { return is_caller_; }

 private:
  const bool is_caller_;
};

// Fake media transport factory creates fake media transport.
class FakeMediaTransportFactory : public MediaTransportFactory {
 public:
  FakeMediaTransportFactory() = default;
  ~FakeMediaTransportFactory() = default;

  RTCErrorOr<std::unique_ptr<MediaTransportInterface>> CreateMediaTransport(
      rtc::PacketTransportInternal* packet_transport,
      rtc::Thread* network_thread,
      bool is_caller) override {
    std::unique_ptr<MediaTransportInterface> media_transport =
        absl::make_unique<FakeMediaTransport>(is_caller);

    return std::move(media_transport);
  }
};

}  // namespace webrtc

#endif  // API_TEST_FAKE_MEDIA_TRANSPORT_H_
