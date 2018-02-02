/*
 * Copyright (C) 2016 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.google.android.exoplayer2.playbacktests.gts;

import android.test.ActivityInstrumentationTestCase2;
import com.google.android.exoplayer2.ExoPlayer;
import com.google.android.exoplayer2.Player;
import com.google.android.exoplayer2.mediacodec.MediaCodecInfo;
import com.google.android.exoplayer2.mediacodec.MediaCodecUtil;
import com.google.android.exoplayer2.mediacodec.MediaCodecUtil.DecoderQueryException;
import com.google.android.exoplayer2.testutil.ActionSchedule;
import com.google.android.exoplayer2.testutil.HostActivity;
import com.google.android.exoplayer2.util.MimeTypes;
import com.google.android.exoplayer2.util.Util;

/**
 * Tests DASH playbacks using {@link ExoPlayer}.
 */
public final class DashStreamingTest extends ActivityInstrumentationTestCase2<HostActivity> {

  private static final String TAG = "DashStreamingTest";

  private static final ActionSchedule SEEKING_SCHEDULE = new ActionSchedule.Builder(TAG)
      .waitForPlaybackState(Player.STATE_READY)
      .delay(10000).seekAndWait(15000)
      .delay(10000).seek(30000).seek(31000).seek(32000).seek(33000).seekAndWait(34000)
      .delay(1000).pause().delay(1000).play()
      .delay(1000).pause().seekAndWait(120000).delay(1000).play()
      .build();
  private static final ActionSchedule RENDERER_DISABLING_SCHEDULE = new ActionSchedule.Builder(TAG)
      .waitForPlaybackState(Player.STATE_READY)
      // Wait 10 seconds, disable the video renderer, wait another 10 seconds and enable it again.
      .delay(10000).disableRenderer(DashTestRunner.VIDEO_RENDERER_INDEX)
      .delay(10000).enableRenderer(DashTestRunner.VIDEO_RENDERER_INDEX)
      // Ditto for the audio renderer.
      .delay(10000).disableRenderer(DashTestRunner.AUDIO_RENDERER_INDEX)
      .delay(10000).enableRenderer(DashTestRunner.AUDIO_RENDERER_INDEX)
      // Wait 10 seconds, then disable and enable the video renderer 5 times in quick succession.
      .delay(10000).disableRenderer(DashTestRunner.VIDEO_RENDERER_INDEX)
      .enableRenderer(DashTestRunner.VIDEO_RENDERER_INDEX)
      .disableRenderer(DashTestRunner.VIDEO_RENDERER_INDEX)
      .enableRenderer(DashTestRunner.VIDEO_RENDERER_INDEX)
      .disableRenderer(DashTestRunner.VIDEO_RENDERER_INDEX)
      .enableRenderer(DashTestRunner.VIDEO_RENDERER_INDEX)
      .disableRenderer(DashTestRunner.VIDEO_RENDERER_INDEX)
      .enableRenderer(DashTestRunner.VIDEO_RENDERER_INDEX)
      .disableRenderer(DashTestRunner.VIDEO_RENDERER_INDEX)
      .enableRenderer(DashTestRunner.VIDEO_RENDERER_INDEX)
      // Ditto for the audio renderer.
      .delay(10000).disableRenderer(DashTestRunner.AUDIO_RENDERER_INDEX)
      .enableRenderer(DashTestRunner.AUDIO_RENDERER_INDEX)
      .disableRenderer(DashTestRunner.AUDIO_RENDERER_INDEX)
      .enableRenderer(DashTestRunner.AUDIO_RENDERER_INDEX)
      .disableRenderer(DashTestRunner.AUDIO_RENDERER_INDEX)
      .enableRenderer(DashTestRunner.AUDIO_RENDERER_INDEX)
      .disableRenderer(DashTestRunner.AUDIO_RENDERER_INDEX)
      .enableRenderer(DashTestRunner.AUDIO_RENDERER_INDEX)
      .disableRenderer(DashTestRunner.AUDIO_RENDERER_INDEX)
      .enableRenderer(DashTestRunner.AUDIO_RENDERER_INDEX)
      // Wait 10 seconds, detach the surface, wait another 10 seconds and attach it again.
      .delay(10000).clearVideoSurface()
      .delay(10000).setVideoSurface()
      // Wait 10 seconds, then seek to near end.
      .delay(10000).seek(120000)
      .build();
  
  private DashTestRunner testRunner;

  public DashStreamingTest() {
    super(HostActivity.class);
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    testRunner = new DashTestRunner(TAG, getActivity(), getInstrumentation());
  }

  @Override
  protected void tearDown() throws Exception {
    testRunner = null;
    super.tearDown();
  }

  // H264 CDD.

  public void testH264Fixed() {
    if (Util.SDK_INT < 16) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_h264_fixed")
        .setManifestUrl(DashTestData.H264_MANIFEST)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(false)
        .setAudioVideoFormats(DashTestData.AAC_AUDIO_REPRESENTATION_ID, DashTestData.H264_CDD_FIXED)
        .run();
  }

  public void testH264Adaptive() throws DecoderQueryException {
    if (Util.SDK_INT < 16 || shouldSkipAdaptiveTest(MimeTypes.VIDEO_H264)) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_h264_adaptive")
        .setManifestUrl(DashTestData.H264_MANIFEST)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(true)
        .setAudioVideoFormats(DashTestData.AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.H264_CDD_ADAPTIVE)
        .run();
  }

  public void testH264AdaptiveWithSeeking() throws DecoderQueryException {
    if (Util.SDK_INT < 16 || shouldSkipAdaptiveTest(MimeTypes.VIDEO_H264)) {
      // Pass.
      return;
    }
    final String streamName = "test_h264_adaptive_with_seeking";
    testRunner
        .setStreamName(streamName)
        .setManifestUrl(DashTestData.H264_MANIFEST)
        .setFullPlaybackNoSeeking(false)
        .setCanIncludeAdditionalVideoFormats(true)
        .setActionSchedule(SEEKING_SCHEDULE)
        .setAudioVideoFormats(DashTestData.AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.H264_CDD_ADAPTIVE)
        .run();
  }

  public void testH264AdaptiveWithRendererDisabling() throws DecoderQueryException {
    if (Util.SDK_INT < 16 || shouldSkipAdaptiveTest(MimeTypes.VIDEO_H264)) {
      // Pass.
      return;
    }
    final String streamName = "test_h264_adaptive_with_renderer_disabling";
    testRunner
        .setStreamName(streamName)
        .setManifestUrl(DashTestData.H264_MANIFEST)
        .setFullPlaybackNoSeeking(false)
        .setCanIncludeAdditionalVideoFormats(true)
        .setActionSchedule(RENDERER_DISABLING_SCHEDULE)
        .setAudioVideoFormats(DashTestData.AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.H264_CDD_ADAPTIVE)
        .run();
  }

  // H265 CDD.

  public void testH265FixedV23() {
    if (Util.SDK_INT < 23) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_h265_fixed")
        .setManifestUrl(DashTestData.H265_MANIFEST)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(false)
        .setAudioVideoFormats(DashTestData.AAC_AUDIO_REPRESENTATION_ID, DashTestData.H265_CDD_FIXED)
        .run();
  }

  public void testH265AdaptiveV24() throws DecoderQueryException {
    if (Util.SDK_INT < 24) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_h265_adaptive")
        .setManifestUrl(DashTestData.H265_MANIFEST)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(true)
        .setAudioVideoFormats(DashTestData.AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.H265_CDD_ADAPTIVE)
        .run();
  }

  public void testH265AdaptiveWithSeekingV24() throws DecoderQueryException {
    if (Util.SDK_INT < 24) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_h265_adaptive_with_seeking")
        .setManifestUrl(DashTestData.H265_MANIFEST)
        .setFullPlaybackNoSeeking(false)
        .setCanIncludeAdditionalVideoFormats(true)
        .setActionSchedule(SEEKING_SCHEDULE)
        .setAudioVideoFormats(DashTestData.AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.H265_CDD_ADAPTIVE)
        .run();
  }

  public void testH265AdaptiveWithRendererDisablingV24() throws DecoderQueryException {
    if (Util.SDK_INT < 24) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_h265_adaptive_with_renderer_disabling")
        .setManifestUrl(DashTestData.H265_MANIFEST)
        .setFullPlaybackNoSeeking(false)
        .setCanIncludeAdditionalVideoFormats(true)
        .setActionSchedule(RENDERER_DISABLING_SCHEDULE)
        .setAudioVideoFormats(DashTestData.AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.H265_CDD_ADAPTIVE)
        .run();
  }

  // VP9 (CDD).

  public void testVp9Fixed360pV23() {
    if (Util.SDK_INT < 23) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_vp9_fixed_360p")
        .setManifestUrl(DashTestData.VP9_MANIFEST)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(false)
        .setAudioVideoFormats(DashTestData.VP9_VORBIS_AUDIO_REPRESENTATION_ID,
            DashTestData.VP9_CDD_FIXED)
        .run();
  }

  public void testVp9AdaptiveV24() throws DecoderQueryException {
    if (Util.SDK_INT < 24) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_vp9_adaptive")
        .setManifestUrl(DashTestData.VP9_MANIFEST)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(true)
        .setAudioVideoFormats(DashTestData.VP9_VORBIS_AUDIO_REPRESENTATION_ID,
            DashTestData.VP9_CDD_ADAPTIVE)
        .run();
  }

  public void testVp9AdaptiveWithSeekingV24() throws DecoderQueryException {
    if (Util.SDK_INT < 24) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_vp9_adaptive_with_seeking")
        .setManifestUrl(DashTestData.VP9_MANIFEST)
        .setFullPlaybackNoSeeking(false)
        .setCanIncludeAdditionalVideoFormats(true)
        .setActionSchedule(SEEKING_SCHEDULE)
        .setAudioVideoFormats(DashTestData.VP9_VORBIS_AUDIO_REPRESENTATION_ID,
            DashTestData.VP9_CDD_ADAPTIVE)
        .run();
  }

  public void testVp9AdaptiveWithRendererDisablingV24() throws DecoderQueryException {
    if (Util.SDK_INT < 24) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_vp9_adaptive_with_renderer_disabling")
        .setManifestUrl(DashTestData.VP9_MANIFEST)
        .setFullPlaybackNoSeeking(false)
        .setCanIncludeAdditionalVideoFormats(true)
        .setActionSchedule(RENDERER_DISABLING_SCHEDULE)
        .setAudioVideoFormats(DashTestData.VP9_VORBIS_AUDIO_REPRESENTATION_ID,
            DashTestData.VP9_CDD_ADAPTIVE)
        .run();
  }

  // H264: Other frame-rates for output buffer count assertions.

  // 23.976 fps.
  public void test23FpsH264FixedV23() {
    if (Util.SDK_INT < 23) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_23fps_h264_fixed")
        .setManifestUrl(DashTestData.H264_23_MANIFEST)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(false)
        .setAudioVideoFormats(DashTestData.AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.H264_BASELINE_480P_23FPS_VIDEO_REPRESENTATION_ID)
        .run();
  }

  // 24 fps.
  public void test24FpsH264FixedV23() {
    if (Util.SDK_INT < 23) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_24fps_h264_fixed")
        .setManifestUrl(DashTestData.H264_24_MANIFEST)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(false)
        .setAudioVideoFormats(DashTestData.AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.H264_BASELINE_480P_24FPS_VIDEO_REPRESENTATION_ID)
        .run();
  }

  // 29.97 fps.
  public void test29FpsH264FixedV23() {
    if (Util.SDK_INT < 23) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_29fps_h264_fixed")
        .setManifestUrl(DashTestData.H264_29_MANIFEST)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(false)
        .setAudioVideoFormats(DashTestData.AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.H264_BASELINE_480P_29FPS_VIDEO_REPRESENTATION_ID)
        .run();
  }

  // Widevine encrypted media tests.
  // H264 CDD.

  public void testWidevineH264FixedV18() throws DecoderQueryException {
    if (Util.SDK_INT < 18) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_widevine_h264_fixed")
        .setManifestUrl(DashTestData.WIDEVINE_H264_MANIFEST)
        .setWidevineInfo(MimeTypes.VIDEO_H264, true)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(false)
        .setAudioVideoFormats(DashTestData.WIDEVINE_AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.WIDEVINE_H264_CDD_FIXED)
        .run();
  }

  public void testWidevineH264AdaptiveV18() throws DecoderQueryException {
    if (Util.SDK_INT < 18 || shouldSkipAdaptiveTest(MimeTypes.VIDEO_H264)) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_widevine_h264_adaptive")
        .setManifestUrl(DashTestData.WIDEVINE_H264_MANIFEST)
        .setWidevineInfo(MimeTypes.VIDEO_H264, true)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(true)
        .setAudioVideoFormats(DashTestData.WIDEVINE_AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.WIDEVINE_H264_CDD_ADAPTIVE)
        .run();
  }

  public void testWidevineH264AdaptiveWithSeekingV18() throws DecoderQueryException {
    if (Util.SDK_INT < 18 || shouldSkipAdaptiveTest(MimeTypes.VIDEO_H264)) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_widevine_h264_adaptive_with_seeking")
        .setManifestUrl(DashTestData.WIDEVINE_H264_MANIFEST)
        .setWidevineInfo(MimeTypes.VIDEO_H264, true)
        .setFullPlaybackNoSeeking(false)
        .setCanIncludeAdditionalVideoFormats(true)
        .setActionSchedule(SEEKING_SCHEDULE)
        .setAudioVideoFormats(DashTestData.WIDEVINE_AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.WIDEVINE_H264_CDD_ADAPTIVE)
        .run();
  }

  public void testWidevineH264AdaptiveWithRendererDisablingV18() throws DecoderQueryException {
    if (Util.SDK_INT < 18 || shouldSkipAdaptiveTest(MimeTypes.VIDEO_H264)) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_widevine_h264_adaptive_with_renderer_disabling")
        .setManifestUrl(DashTestData.WIDEVINE_H264_MANIFEST)
        .setWidevineInfo(MimeTypes.VIDEO_H264, true)
        .setFullPlaybackNoSeeking(false)
        .setCanIncludeAdditionalVideoFormats(true)
        .setActionSchedule(RENDERER_DISABLING_SCHEDULE)
        .setAudioVideoFormats(DashTestData.WIDEVINE_AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.WIDEVINE_H264_CDD_ADAPTIVE)
        .run();
  }

  // H265 CDD.

  public void testWidevineH265FixedV23() throws DecoderQueryException {
    if (Util.SDK_INT < 23) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_widevine_h265_fixed")
        .setManifestUrl(DashTestData.WIDEVINE_H265_MANIFEST)
        .setWidevineInfo(MimeTypes.VIDEO_H265, true)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(false)
        .setAudioVideoFormats(DashTestData.WIDEVINE_AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.WIDEVINE_H265_CDD_FIXED)
        .run();
  }

  public void testWidevineH265AdaptiveV24() throws DecoderQueryException {
    if (Util.SDK_INT < 24) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_widevine_h265_adaptive")
        .setManifestUrl(DashTestData.WIDEVINE_H265_MANIFEST)
        .setWidevineInfo(MimeTypes.VIDEO_H265, true)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(true)
        .setAudioVideoFormats(DashTestData.WIDEVINE_AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.WIDEVINE_H265_CDD_ADAPTIVE)
        .run();
  }

  public void testWidevineH265AdaptiveWithSeekingV24() throws DecoderQueryException {
    if (Util.SDK_INT < 24) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_widevine_h265_adaptive_with_seeking")
        .setManifestUrl(DashTestData.WIDEVINE_H265_MANIFEST)
        .setWidevineInfo(MimeTypes.VIDEO_H265, true)
        .setFullPlaybackNoSeeking(false)
        .setCanIncludeAdditionalVideoFormats(true)
        .setActionSchedule(SEEKING_SCHEDULE)
        .setAudioVideoFormats(DashTestData.WIDEVINE_AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.WIDEVINE_H265_CDD_ADAPTIVE)
        .run();
  }

  public void testWidevineH265AdaptiveWithRendererDisablingV24() throws DecoderQueryException {
    if (Util.SDK_INT < 24) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_widevine_h265_adaptive_with_renderer_disabling")
        .setManifestUrl(DashTestData.WIDEVINE_H265_MANIFEST)
        .setWidevineInfo(MimeTypes.VIDEO_H265, true)
        .setFullPlaybackNoSeeking(false)
        .setCanIncludeAdditionalVideoFormats(true)
        .setActionSchedule(RENDERER_DISABLING_SCHEDULE)
        .setAudioVideoFormats(DashTestData.WIDEVINE_AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.WIDEVINE_H265_CDD_ADAPTIVE)
        .run();
  }

  // VP9 (CDD).

  public void testWidevineVp9Fixed360pV23() throws DecoderQueryException {
    if (Util.SDK_INT < 23) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_widevine_vp9_fixed_360p")
        .setManifestUrl(DashTestData.WIDEVINE_VP9_MANIFEST)
        .setWidevineInfo(MimeTypes.VIDEO_VP9, true)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(false)
        .setAudioVideoFormats(DashTestData.WIDEVINE_VP9_AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.WIDEVINE_VP9_CDD_FIXED)
        .run();
  }

  public void testWidevineVp9AdaptiveV24() throws DecoderQueryException {
    if (Util.SDK_INT < 24) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_widevine_vp9_adaptive")
        .setManifestUrl(DashTestData.WIDEVINE_VP9_MANIFEST)
        .setWidevineInfo(MimeTypes.VIDEO_VP9, true)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(true)
        .setAudioVideoFormats(DashTestData.WIDEVINE_VP9_AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.WIDEVINE_VP9_CDD_ADAPTIVE)
        .run();
  }

  public void testWidevineVp9AdaptiveWithSeekingV24() throws DecoderQueryException {
    if (Util.SDK_INT < 24) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_widevine_vp9_adaptive_with_seeking")
        .setManifestUrl(DashTestData.WIDEVINE_VP9_MANIFEST)
        .setWidevineInfo(MimeTypes.VIDEO_VP9, true)
        .setFullPlaybackNoSeeking(false)
        .setCanIncludeAdditionalVideoFormats(true)
        .setActionSchedule(SEEKING_SCHEDULE)
        .setAudioVideoFormats(DashTestData.WIDEVINE_VP9_AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.WIDEVINE_VP9_CDD_ADAPTIVE)
        .run();
  }

  public void testWidevineVp9AdaptiveWithRendererDisablingV24() throws DecoderQueryException {
    if (Util.SDK_INT < 24) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_widevine_vp9_adaptive_with_renderer_disabling")
        .setManifestUrl(DashTestData.WIDEVINE_VP9_MANIFEST)
        .setWidevineInfo(MimeTypes.VIDEO_VP9, true)
        .setFullPlaybackNoSeeking(false)
        .setCanIncludeAdditionalVideoFormats(true)
        .setActionSchedule(RENDERER_DISABLING_SCHEDULE)
        .setAudioVideoFormats(DashTestData.WIDEVINE_VP9_AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.WIDEVINE_VP9_CDD_ADAPTIVE)
        .run();
  }

  // H264: Other frame-rates for output buffer count assertions.

  // 23.976 fps.
  public void testWidevine23FpsH264FixedV23() throws DecoderQueryException {
    if (Util.SDK_INT < 23) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_widevine_23fps_h264_fixed")
        .setManifestUrl(DashTestData.WIDEVINE_H264_23_MANIFEST)
        .setWidevineInfo(MimeTypes.VIDEO_H264, true)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(false)
        .setAudioVideoFormats(DashTestData.WIDEVINE_AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.WIDEVINE_H264_BASELINE_480P_23FPS_VIDEO_REPRESENTATION_ID)
        .run();
  }

  // 24 fps.
  public void testWidevine24FpsH264FixedV23() throws DecoderQueryException {
    if (Util.SDK_INT < 23) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_widevine_24fps_h264_fixed")
        .setManifestUrl(DashTestData.WIDEVINE_H264_24_MANIFEST)
        .setWidevineInfo(MimeTypes.VIDEO_H264, true)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(false)
        .setAudioVideoFormats(DashTestData.WIDEVINE_AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.WIDEVINE_H264_BASELINE_480P_24FPS_VIDEO_REPRESENTATION_ID)
        .run();
  }

  // 29.97 fps.
  public void testWidevine29FpsH264FixedV23() throws DecoderQueryException {
    if (Util.SDK_INT < 23) {
      // Pass.
      return;
    }
    testRunner
        .setStreamName("test_widevine_29fps_h264_fixed")
        .setManifestUrl(DashTestData.WIDEVINE_H264_29_MANIFEST)
        .setWidevineInfo(MimeTypes.VIDEO_H264, true)
        .setFullPlaybackNoSeeking(true)
        .setCanIncludeAdditionalVideoFormats(false)
        .setAudioVideoFormats(DashTestData.WIDEVINE_AAC_AUDIO_REPRESENTATION_ID,
            DashTestData.WIDEVINE_H264_BASELINE_480P_29FPS_VIDEO_REPRESENTATION_ID)
        .run();
  }

  // Decoder info.

  public void testDecoderInfoH264() throws DecoderQueryException {
    if (Util.SDK_INT < 16) {
      // Pass.
      return;
    }
    MediaCodecInfo decoderInfo = MediaCodecUtil.getDecoderInfo(MimeTypes.VIDEO_H264, false);
    assertNotNull(decoderInfo);
    assertTrue(Util.SDK_INT < 21 || decoderInfo.adaptive);
  }

  public void testDecoderInfoH265V24() throws DecoderQueryException {
    if (Util.SDK_INT < 24) {
      // Pass.
      return;
    }
    assertTrue(MediaCodecUtil.getDecoderInfo(MimeTypes.VIDEO_H265, false).adaptive);
  }

  public void testDecoderInfoVP9V24() throws DecoderQueryException {
    if (Util.SDK_INT < 24) {
      // Pass.
      return;
    }
    assertTrue(MediaCodecUtil.getDecoderInfo(MimeTypes.VIDEO_VP9, false).adaptive);
  }

  // Internal.

  private static boolean shouldSkipAdaptiveTest(String mimeType) throws DecoderQueryException {
    MediaCodecInfo decoderInfo = MediaCodecUtil.getDecoderInfo(mimeType, false);
    return decoderInfo == null || !decoderInfo.adaptive;
  }

}
