/*
 *  Copyright (c) 2016 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */

#include "media/engine/internaldecoderfactory.h"

#include "api/video_codecs/sdp_video_format.h"
#include "media/base/mediaconstants.h"
#include "modules/video_coding/codecs/h264/include/h264.h"
#include "modules/video_coding/codecs/vp8/include/vp8.h"
#include "modules/video_coding/codecs/vp9/include/vp9.h"
#include "rtc_base/checks.h"
#include "rtc_base/logging.h"

namespace webrtc {

namespace {

bool IsFormatSupported(
    const std::vector<webrtc::SdpVideoFormat>& supported_formats,
    const webrtc::SdpVideoFormat& format) {
  for (const webrtc::SdpVideoFormat& supported_format : supported_formats) {
    if (cricket::IsSameCodec(format.name, format.parameters,
                             supported_format.name,
                             supported_format.parameters)) {
      return true;
    }
  }
  return false;
}

}  // namespace

std::vector<SdpVideoFormat> InternalDecoderFactory::GetSupportedFormats()
    const {
  std::vector<SdpVideoFormat> formats;
  formats.push_back(SdpVideoFormat(cricket::kVp8CodecName));
  for (const SdpVideoFormat& format : SupportedVP9Codecs())
    formats.push_back(format);
  for (const SdpVideoFormat& h264_format : SupportedH264Codecs())
    formats.push_back(h264_format);
  return formats;
}

std::unique_ptr<VideoDecoder> InternalDecoderFactory::CreateVideoDecoder(
    const SdpVideoFormat& format) {
  if (!IsFormatSupported(GetSupportedFormats(), format)) {
    RTC_LOG(LS_ERROR) << "Trying to create decoder for unsupported format";
    return nullptr;
  }

  if (cricket::CodecNamesEq(format.name, cricket::kVp8CodecName))
    return VP8Decoder::Create();
  if (cricket::CodecNamesEq(format.name, cricket::kVp9CodecName))
    return VP9Decoder::Create();
  if (cricket::CodecNamesEq(format.name, cricket::kH264CodecName))
    return H264Decoder::Create();

  RTC_NOTREACHED();
  return nullptr;
}

}  // namespace webrtc
