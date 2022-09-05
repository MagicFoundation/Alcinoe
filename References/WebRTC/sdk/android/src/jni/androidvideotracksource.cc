/*
 *  Copyright (c) 2016 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */

#include "sdk/android/src/jni/androidvideotracksource.h"

#include <utility>

#include "api/videosourceproxy.h"
#include "rtc_base/logging.h"

namespace webrtc {
namespace jni {

namespace {
// MediaCodec wants resolution to be divisible by 2.
const int kRequiredResolutionAlignment = 2;
}  // namespace

AndroidVideoTrackSource::AndroidVideoTrackSource(rtc::Thread* signaling_thread,
                                                 JNIEnv* jni,
                                                 bool is_screencast)
    : AdaptedVideoTrackSource(kRequiredResolutionAlignment),
      signaling_thread_(signaling_thread),
      is_screencast_(is_screencast) {
  RTC_LOG(LS_INFO) << "AndroidVideoTrackSource ctor";
  camera_thread_checker_.DetachFromThread();
}
AndroidVideoTrackSource::~AndroidVideoTrackSource() = default;

bool AndroidVideoTrackSource::is_screencast() const {
  return is_screencast_;
}

absl::optional<bool> AndroidVideoTrackSource::needs_denoising() const {
  return false;
}

void AndroidVideoTrackSource::SetState(SourceState state) {
  if (rtc::Thread::Current() != signaling_thread_) {
    invoker_.AsyncInvoke<void>(
        RTC_FROM_HERE, signaling_thread_,
        rtc::Bind(&AndroidVideoTrackSource::SetState, this, state));
    return;
  }

  if (state_ != state) {
    state_ = state;
    FireOnChanged();
  }
}

AndroidVideoTrackSource::SourceState AndroidVideoTrackSource::state() const {
  return state_;
}

bool AndroidVideoTrackSource::remote() const {
  return false;
}

void AndroidVideoTrackSource::OnFrameCaptured(
    JNIEnv* jni,
    int width,
    int height,
    int64_t timestamp_ns,
    VideoRotation rotation,
    const JavaRef<jobject>& j_video_frame_buffer) {
  RTC_DCHECK(camera_thread_checker_.CalledOnValidThread());

  int64_t camera_time_us = timestamp_ns / rtc::kNumNanosecsPerMicrosec;
  int64_t translated_camera_time_us =
      timestamp_aligner_.TranslateTimestamp(camera_time_us, rtc::TimeMicros());

  int adapted_width;
  int adapted_height;
  int crop_width;
  int crop_height;
  int crop_x;
  int crop_y;

  if (!AdaptFrame(width, height, camera_time_us, &adapted_width,
                  &adapted_height, &crop_width, &crop_height, &crop_x,
                  &crop_y)) {
    return;
  }

  rtc::scoped_refptr<VideoFrameBuffer> buffer =
      AndroidVideoBuffer::Create(jni, j_video_frame_buffer)
          ->CropAndScale(jni, crop_x, crop_y, crop_width, crop_height,
                         adapted_width, adapted_height);

  // AdaptedVideoTrackSource handles applying rotation for I420 frames.
  if (apply_rotation() && rotation != kVideoRotation_0) {
    buffer = buffer->ToI420();
  }

  OnFrame(VideoFrame(buffer, rotation, translated_camera_time_us));
}

void AndroidVideoTrackSource::OnOutputFormatRequest(int width,
                                                    int height,
                                                    int fps) {
  cricket::VideoFormat format(width, height,
                              cricket::VideoFormat::FpsToInterval(fps), 0);
  video_adapter()->OnOutputFormatRequest(format);
}

}  // namespace jni
}  // namespace webrtc
