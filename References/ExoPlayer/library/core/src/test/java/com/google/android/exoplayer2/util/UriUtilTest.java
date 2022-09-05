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
package com.google.android.exoplayer2.util;

import static com.google.android.exoplayer2.util.UriUtil.resolve;
import static com.google.common.truth.Truth.assertThat;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

/**
 * Unit tests for {@link UriUtil}.
 */
@RunWith(RobolectricTestRunner.class)
@Config(sdk = Config.TARGET_SDK, manifest = Config.NONE)
public final class UriUtilTest {

  /**
   * Tests normal usage of {@link UriUtil#resolve(String, String)}.
   * <p>
   * The test cases are taken from RFC-3986 5.4.1.
   */
  @Test
  public void testResolveNormal() {
    String base = "http://a/b/c/d;p?q";

    assertThat(resolve(base, "g:h")).isEqualTo("g:h");
    assertThat(resolve(base, "g")).isEqualTo("http://a/b/c/g");
    assertThat(resolve(base, "g/")).isEqualTo("http://a/b/c/g/");
    assertThat(resolve(base, "/g")).isEqualTo("http://a/g");
    assertThat(resolve(base, "//g")).isEqualTo("http://g");
    assertThat(resolve(base, "?y")).isEqualTo("http://a/b/c/d;p?y");
    assertThat(resolve(base, "g?y")).isEqualTo("http://a/b/c/g?y");
    assertThat(resolve(base, "#s")).isEqualTo("http://a/b/c/d;p?q#s");
    assertThat(resolve(base, "g#s")).isEqualTo("http://a/b/c/g#s");
    assertThat(resolve(base, "g?y#s")).isEqualTo("http://a/b/c/g?y#s");
    assertThat(resolve(base, ";x")).isEqualTo("http://a/b/c/;x");
    assertThat(resolve(base, "g;x")).isEqualTo("http://a/b/c/g;x");
    assertThat(resolve(base, "g;x?y#s")).isEqualTo("http://a/b/c/g;x?y#s");
    assertThat(resolve(base, "")).isEqualTo("http://a/b/c/d;p?q");
    assertThat(resolve(base, ".")).isEqualTo("http://a/b/c/");
    assertThat(resolve(base, "./")).isEqualTo("http://a/b/c/");
    assertThat(resolve(base, "..")).isEqualTo("http://a/b/");
    assertThat(resolve(base, "../")).isEqualTo("http://a/b/");
    assertThat(resolve(base, "../g")).isEqualTo("http://a/b/g");
    assertThat(resolve(base, "../..")).isEqualTo("http://a/");
    assertThat(resolve(base, "../../")).isEqualTo("http://a/");
    assertThat(resolve(base, "../../g")).isEqualTo("http://a/g");
  }

  /**
   * Tests abnormal usage of {@link UriUtil#resolve(String, String)}.
   * <p>
   * The test cases are taken from RFC-3986 5.4.2.
   */
  @Test
  public void testResolveAbnormal() {
    String base = "http://a/b/c/d;p?q";

    assertThat(resolve(base, "../../../g")).isEqualTo("http://a/g");
    assertThat(resolve(base, "../../../../g")).isEqualTo("http://a/g");

    assertThat(resolve(base, "/./g")).isEqualTo("http://a/g");
    assertThat(resolve(base, "/../g")).isEqualTo("http://a/g");
    assertThat(resolve(base, "g.")).isEqualTo("http://a/b/c/g.");
    assertThat(resolve(base, ".g")).isEqualTo("http://a/b/c/.g");
    assertThat(resolve(base, "g..")).isEqualTo("http://a/b/c/g..");
    assertThat(resolve(base, "..g")).isEqualTo("http://a/b/c/..g");

    assertThat(resolve(base, "./../g")).isEqualTo("http://a/b/g");
    assertThat(resolve(base, "./g/.")).isEqualTo("http://a/b/c/g/");
    assertThat(resolve(base, "g/./h")).isEqualTo("http://a/b/c/g/h");
    assertThat(resolve(base, "g/../h")).isEqualTo("http://a/b/c/h");
    assertThat(resolve(base, "g;x=1/./y")).isEqualTo("http://a/b/c/g;x=1/y");
    assertThat(resolve(base, "g;x=1/../y")).isEqualTo("http://a/b/c/y");

    assertThat(resolve(base, "g?y/./x")).isEqualTo("http://a/b/c/g?y/./x");
    assertThat(resolve(base, "g?y/../x")).isEqualTo("http://a/b/c/g?y/../x");
    assertThat(resolve(base, "g#s/./x")).isEqualTo("http://a/b/c/g#s/./x");
    assertThat(resolve(base, "g#s/../x")).isEqualTo("http://a/b/c/g#s/../x");

    assertThat(resolve(base, "http:g")).isEqualTo("http:g");
  }

  /**
   * Tests additional abnormal usage of {@link UriUtil#resolve(String, String)}.
   */
  @Test
  public void testResolveAbnormalAdditional() {
    assertThat(resolve("http://a/b", "c:d/../e")).isEqualTo("c:e");
    assertThat(resolve("a:b", "../c")).isEqualTo("a:c");
  }

}
