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
package com.google.android.exoplayer2.source.dash.manifest;

import com.google.android.exoplayer2.C;
import junit.framework.TestCase;

/**
 * Unit test for {@link RangedUri}.
 */
public class RangedUriTest extends TestCase {

  private static final String BASE_URI = "http://www.test.com/";
  private static final String PARTIAL_URI = "path/file.ext";
  private static final String FULL_URI = BASE_URI + PARTIAL_URI;

  public void testMerge() {
    RangedUri rangeA = new RangedUri(FULL_URI, 0, 10);
    RangedUri rangeB = new RangedUri(FULL_URI, 10, 10);
    RangedUri expected = new RangedUri(FULL_URI, 0, 20);
    assertMerge(rangeA, rangeB, expected, null);
  }

  public void testMergeUnbounded() {
    RangedUri rangeA = new RangedUri(FULL_URI, 0, 10);
    RangedUri rangeB = new RangedUri(FULL_URI, 10, C.LENGTH_UNSET);
    RangedUri expected = new RangedUri(FULL_URI, 0, C.LENGTH_UNSET);
    assertMerge(rangeA, rangeB, expected, null);
  }

  public void testNonMerge() {
    // A and B do not overlap, so should not merge
    RangedUri rangeA = new RangedUri(FULL_URI, 0, 10);
    RangedUri rangeB = new RangedUri(FULL_URI, 11, 10);
    assertNonMerge(rangeA, rangeB, null);

    // A and B do not overlap, so should not merge
    rangeA = new RangedUri(FULL_URI, 0, 10);
    rangeB = new RangedUri(FULL_URI, 11, C.LENGTH_UNSET);
    assertNonMerge(rangeA, rangeB, null);

    // A and B are bounded but overlap, so should not merge
    rangeA = new RangedUri(FULL_URI, 0, 11);
    rangeB = new RangedUri(FULL_URI, 10, 10);
    assertNonMerge(rangeA, rangeB, null);

    // A and B overlap due to unboundedness, so should not merge
    rangeA = new RangedUri(FULL_URI, 0, C.LENGTH_UNSET);
    rangeB = new RangedUri(FULL_URI, 10, C.LENGTH_UNSET);
    assertNonMerge(rangeA, rangeB, null);
  }

  public void testMergeWithBaseUri() {
    RangedUri rangeA = new RangedUri(PARTIAL_URI, 0, 10);
    RangedUri rangeB = new RangedUri(FULL_URI, 10, 10);
    RangedUri expected = new RangedUri(FULL_URI, 0, 20);
    assertMerge(rangeA, rangeB, expected, BASE_URI);
  }

  private void assertMerge(RangedUri rangeA, RangedUri rangeB, RangedUri expected, String baseUrl) {
    RangedUri merged = rangeA.attemptMerge(rangeB, baseUrl);
    assertEquals(expected, merged);
    merged = rangeB.attemptMerge(rangeA, baseUrl);
    assertEquals(expected, merged);
  }

  private void assertNonMerge(RangedUri rangeA, RangedUri rangeB, String baseUrl) {
    RangedUri merged = rangeA.attemptMerge(rangeB, baseUrl);
    assertNull(merged);
    merged = rangeB.attemptMerge(rangeA, baseUrl);
    assertNull(merged);
  }

}
