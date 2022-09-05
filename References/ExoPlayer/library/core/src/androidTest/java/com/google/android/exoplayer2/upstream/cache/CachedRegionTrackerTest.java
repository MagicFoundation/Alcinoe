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
package com.google.android.exoplayer2.upstream.cache;

import android.test.InstrumentationTestCase;
import com.google.android.exoplayer2.extractor.ChunkIndex;
import com.google.android.exoplayer2.testutil.MockitoUtil;
import com.google.android.exoplayer2.util.Util;
import java.io.File;
import java.io.IOException;
import org.mockito.Mock;

/**
 * Tests for {@link CachedRegionTracker}.
 */
public final class CachedRegionTrackerTest extends InstrumentationTestCase {

  private static final String CACHE_KEY = "abc";
  private static final long MS_IN_US = 1000;

  // 5 chunks, each 20 bytes long and 100 ms long.
  private static final ChunkIndex CHUNK_INDEX = new ChunkIndex(
      new int[] {20, 20, 20, 20, 20},
      new long[] {100, 120, 140, 160, 180},
      new long[] {100 * MS_IN_US, 100 * MS_IN_US, 100 * MS_IN_US, 100 * MS_IN_US, 100 * MS_IN_US},
      new long[] {0, 100 * MS_IN_US, 200 * MS_IN_US, 300 * MS_IN_US, 400 * MS_IN_US});

  @Mock private Cache cache;
  private CachedRegionTracker tracker;

  private CachedContentIndex index;
  private File cacheDir;

  @Override
  protected void setUp() throws Exception {
    MockitoUtil.setUpMockito(this);

    tracker = new CachedRegionTracker(cache, CACHE_KEY, CHUNK_INDEX);
    cacheDir = Util.createTempDirectory(getInstrumentation().getContext(), "ExoPlayerTest");
    index = new CachedContentIndex(cacheDir);
  }

  @Override
  protected void tearDown() throws Exception {
    Util.recursiveDelete(cacheDir);
  }

  public void testGetRegion_noSpansInCache() {
    assertEquals(CachedRegionTracker.NOT_CACHED, tracker.getRegionEndTimeMs(100));
    assertEquals(CachedRegionTracker.NOT_CACHED, tracker.getRegionEndTimeMs(150));
  }

  public void testGetRegion_fullyCached() throws Exception {
    tracker.onSpanAdded(
        cache,
        newCacheSpan(100, 100));

    assertEquals(CachedRegionTracker.CACHED_TO_END, tracker.getRegionEndTimeMs(101));
    assertEquals(CachedRegionTracker.CACHED_TO_END, tracker.getRegionEndTimeMs(121));
  }

  public void testGetRegion_partiallyCached() throws Exception {
    tracker.onSpanAdded(
        cache,
        newCacheSpan(100, 40));

    assertEquals(200, tracker.getRegionEndTimeMs(101));
    assertEquals(200, tracker.getRegionEndTimeMs(121));
  }

  public void testGetRegion_multipleSpanAddsJoinedCorrectly() throws Exception {
    tracker.onSpanAdded(
        cache,
        newCacheSpan(100, 20));
    tracker.onSpanAdded(
        cache,
        newCacheSpan(120, 20));

    assertEquals(200, tracker.getRegionEndTimeMs(101));
    assertEquals(200, tracker.getRegionEndTimeMs(121));
  }

  public void testGetRegion_fullyCachedThenPartiallyRemoved() throws Exception {
    // Start with the full stream in cache.
    tracker.onSpanAdded(
        cache,
        newCacheSpan(100, 100));

    // Remove the middle bit.
    tracker.onSpanRemoved(
        cache,
        newCacheSpan(140, 40));

    assertEquals(200, tracker.getRegionEndTimeMs(101));
    assertEquals(200, tracker.getRegionEndTimeMs(121));

    assertEquals(CachedRegionTracker.CACHED_TO_END, tracker.getRegionEndTimeMs(181));
  }

  public void testGetRegion_subchunkEstimation() throws Exception {
    tracker.onSpanAdded(
        cache,
        newCacheSpan(100, 10));

    assertEquals(50, tracker.getRegionEndTimeMs(101));
    assertEquals(CachedRegionTracker.NOT_CACHED, tracker.getRegionEndTimeMs(111));
  }

  private CacheSpan newCacheSpan(int position, int length) throws IOException {
    return SimpleCacheSpanTest.createCacheSpan(index, cacheDir, CACHE_KEY, position, length, 0);
  }

}
