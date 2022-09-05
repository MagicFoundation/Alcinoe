/*
 *  Copyright (c) 2017 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */

#include "modules/audio_processing/aec3/echo_path_delay_estimator.h"

#include <algorithm>
#include <string>

#include "api/audio/echo_canceller3_config.h"
#include "modules/audio_processing/aec3/aec3_common.h"
#include "modules/audio_processing/aec3/render_delay_buffer.h"
#include "modules/audio_processing/logging/apm_data_dumper.h"
#include "modules/audio_processing/test/echo_canceller_test_tools.h"
#include "rtc_base/random.h"
#include "rtc_base/strings/string_builder.h"
#include "test/gtest.h"

namespace webrtc {
namespace {

std::string ProduceDebugText(size_t delay, size_t down_sampling_factor) {
  rtc::StringBuilder ss;
  ss << "Delay: " << delay;
  ss << ", Down sampling factor: " << down_sampling_factor;
  return ss.Release();
}

}  // namespace

// Verifies that the basic API calls work.
TEST(EchoPathDelayEstimator, BasicApiCalls) {
  ApmDataDumper data_dumper(0);
  EchoCanceller3Config config;
  std::unique_ptr<RenderDelayBuffer> render_delay_buffer(
      RenderDelayBuffer::Create2(config, 3));
  EchoPathDelayEstimator estimator(&data_dumper, config);
  std::vector<std::vector<float>> render(3, std::vector<float>(kBlockSize));
  std::vector<float> capture(kBlockSize);
  for (size_t k = 0; k < 100; ++k) {
    render_delay_buffer->Insert(render);
    estimator.EstimateDelay(render_delay_buffer->GetDownsampledRenderBuffer(),
                            capture);
  }
}

// Verifies that the delay estimator produces correct delay for artificially
// delayed signals.
TEST(EchoPathDelayEstimator, DelayEstimation) {
  Random random_generator(42U);
  std::vector<std::vector<float>> render(3, std::vector<float>(kBlockSize));
  std::vector<float> capture(kBlockSize);
  ApmDataDumper data_dumper(0);
  constexpr size_t kDownSamplingFactors[] = {2, 4, 8};
  for (auto down_sampling_factor : kDownSamplingFactors) {
    EchoCanceller3Config config;
    config.delay.down_sampling_factor = down_sampling_factor;
    config.delay.num_filters = 10;
    for (size_t delay_samples : {30, 64, 150, 200, 800, 4000}) {
      SCOPED_TRACE(ProduceDebugText(delay_samples, down_sampling_factor));
      std::unique_ptr<RenderDelayBuffer> render_delay_buffer(
          RenderDelayBuffer::Create2(config, 3));
      DelayBuffer<float> signal_delay_buffer(delay_samples);
      EchoPathDelayEstimator estimator(&data_dumper, config);

      absl::optional<DelayEstimate> estimated_delay_samples;
      for (size_t k = 0; k < (500 + (delay_samples) / kBlockSize); ++k) {
        RandomizeSampleVector(&random_generator, render[0]);
        signal_delay_buffer.Delay(render[0], capture);
        render_delay_buffer->Insert(render);

        if (k == 0) {
          render_delay_buffer->Reset();
        }

        render_delay_buffer->PrepareCaptureProcessing();

        auto estimate = estimator.EstimateDelay(
            render_delay_buffer->GetDownsampledRenderBuffer(), capture);

        if (estimate) {
          estimated_delay_samples = estimate;
        }
      }

      if (estimated_delay_samples) {
        // Allow estimated delay to be off by one sample in the down-sampled
        // domain.
        size_t delay_ds = delay_samples / down_sampling_factor;
        size_t estimated_delay_ds =
            estimated_delay_samples->delay / down_sampling_factor;
        EXPECT_NEAR(delay_ds, estimated_delay_ds, 1);
      } else {
        ADD_FAILURE();
      }
    }
  }
}

// Verifies that the delay estimator does not produce delay estimates for render
// signals of low level.
TEST(EchoPathDelayEstimator, NoDelayEstimatesForLowLevelRenderSignals) {
  Random random_generator(42U);
  EchoCanceller3Config config;
  std::vector<std::vector<float>> render(3, std::vector<float>(kBlockSize));
  std::vector<float> capture(kBlockSize);
  ApmDataDumper data_dumper(0);
  EchoPathDelayEstimator estimator(&data_dumper, config);
  std::unique_ptr<RenderDelayBuffer> render_delay_buffer(
      RenderDelayBuffer::Create2(EchoCanceller3Config(), 3));
  for (size_t k = 0; k < 100; ++k) {
    RandomizeSampleVector(&random_generator, render[0]);
    for (auto& render_k : render[0]) {
      render_k *= 100.f / 32767.f;
    }
    std::copy(render[0].begin(), render[0].end(), capture.begin());
    render_delay_buffer->Insert(render);
    render_delay_buffer->PrepareCaptureProcessing();
    EXPECT_FALSE(estimator.EstimateDelay(
        render_delay_buffer->GetDownsampledRenderBuffer(), capture));
  }
}

#if RTC_DCHECK_IS_ON && GTEST_HAS_DEATH_TEST && !defined(WEBRTC_ANDROID)

// Verifies the check for the render blocksize.
// TODO(peah): Re-enable the test once the issue with memory leaks during DEATH
// tests on test bots has been fixed.
TEST(EchoPathDelayEstimator, DISABLED_WrongRenderBlockSize) {
  ApmDataDumper data_dumper(0);
  EchoCanceller3Config config;
  EchoPathDelayEstimator estimator(&data_dumper, config);
  std::unique_ptr<RenderDelayBuffer> render_delay_buffer(
      RenderDelayBuffer::Create2(config, 3));
  std::vector<float> capture(kBlockSize);
  EXPECT_DEATH(estimator.EstimateDelay(
                   render_delay_buffer->GetDownsampledRenderBuffer(), capture),
               "");
}

// Verifies the check for the capture blocksize.
// TODO(peah): Re-enable the test once the issue with memory leaks during DEATH
// tests on test bots has been fixed.
TEST(EchoPathDelayEstimator, WrongCaptureBlockSize) {
  ApmDataDumper data_dumper(0);
  EchoCanceller3Config config;
  EchoPathDelayEstimator estimator(&data_dumper, config);
  std::unique_ptr<RenderDelayBuffer> render_delay_buffer(
      RenderDelayBuffer::Create2(config, 3));
  std::vector<float> capture(std::vector<float>(kBlockSize - 1));
  EXPECT_DEATH(estimator.EstimateDelay(
                   render_delay_buffer->GetDownsampledRenderBuffer(), capture),
               "");
}

// Verifies the check for non-null data dumper.
TEST(EchoPathDelayEstimator, NullDataDumper) {
  EXPECT_DEATH(EchoPathDelayEstimator(nullptr, EchoCanceller3Config()), "");
}

#endif

}  // namespace webrtc
