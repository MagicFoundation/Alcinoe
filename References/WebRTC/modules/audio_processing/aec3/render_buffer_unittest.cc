/*
 *  Copyright (c) 2017 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */

#include "modules/audio_processing/aec3/render_buffer.h"

#include <algorithm>
#include <functional>
#include <vector>

#include "test/gtest.h"

namespace webrtc {

#if RTC_DCHECK_IS_ON && GTEST_HAS_DEATH_TEST && !defined(WEBRTC_ANDROID)

// Verifies the check for non-null fft buffer.
TEST(RenderBuffer, NullExternalFftBuffer) {
  MatrixBuffer block_buffer(10, 3, kBlockSize);
  VectorBuffer spectrum_buffer(10, kFftLengthBy2Plus1);
  EXPECT_DEATH(RenderBuffer(&block_buffer, &spectrum_buffer, nullptr), "");
}

// Verifies the check for non-null spectrum buffer.
TEST(RenderBuffer, NullExternalSpectrumBuffer) {
  FftBuffer fft_buffer(10);
  MatrixBuffer block_buffer(10, 3, kBlockSize);
  EXPECT_DEATH(RenderBuffer(&block_buffer, nullptr, &fft_buffer), "");
}

// Verifies the check for non-null block buffer.
TEST(RenderBuffer, NullExternalBlockBuffer) {
  FftBuffer fft_buffer(10);
  VectorBuffer spectrum_buffer(10, kFftLengthBy2Plus1);
  EXPECT_DEATH(RenderBuffer(nullptr, &spectrum_buffer, &fft_buffer), "");
}

#endif

}  // namespace webrtc
