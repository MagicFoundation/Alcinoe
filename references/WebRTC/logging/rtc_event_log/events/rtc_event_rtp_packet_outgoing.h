/*
 *  Copyright (c) 2017 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */

#ifndef LOGGING_RTC_EVENT_LOG_EVENTS_RTC_EVENT_RTP_PACKET_OUTGOING_H_
#define LOGGING_RTC_EVENT_LOG_EVENTS_RTC_EVENT_RTP_PACKET_OUTGOING_H_

#include <memory>

#include "logging/rtc_event_log/events/rtc_event.h"
#include "modules/rtp_rtcp/source/rtp_packet.h"

namespace webrtc {

class RtpPacketToSend;

class RtcEventRtpPacketOutgoing final : public RtcEvent {
 public:
  RtcEventRtpPacketOutgoing(const RtpPacketToSend& packet,
                            int probe_cluster_id);
  ~RtcEventRtpPacketOutgoing() override;

  Type GetType() const override;

  bool IsConfigEvent() const override;

  std::unique_ptr<RtcEvent> Copy() const override;

  size_t packet_length() const {
    return payload_length_ + header_length_ + padding_length_;
  }

  RtpPacket header_;            // Only the packet's header will be stored here.
  const size_t payload_length_;  // Media payload, excluding header and padding.
  const size_t header_length_;   // RTP header.
  const size_t padding_length_;  // RTP padding.
  const int probe_cluster_id_;

 private:
  RtcEventRtpPacketOutgoing(const RtcEventRtpPacketOutgoing& other);
};

}  // namespace webrtc

#endif  // LOGGING_RTC_EVENT_LOG_EVENTS_RTC_EVENT_RTP_PACKET_OUTGOING_H_
