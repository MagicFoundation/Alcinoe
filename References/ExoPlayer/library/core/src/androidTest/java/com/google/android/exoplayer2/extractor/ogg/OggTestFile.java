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
package com.google.android.exoplayer2.extractor.ogg;

import com.google.android.exoplayer2.testutil.OggTestData;
import com.google.android.exoplayer2.testutil.TestUtil;
import java.util.ArrayList;
import java.util.Random;
import junit.framework.Assert;

/**
 * Generates test data.
 */
/* package */ final class OggTestFile {

  private static final int MAX_PACKET_LENGTH = 2048;
  private static final int MAX_SEGMENT_COUNT = 10;
  private static final int MAX_GRANULES_IN_PAGE = 100000;

  public final byte[] data;
  public final long lastGranule;
  public final int packetCount;
  public final int pageCount;
  public final int firstPayloadPageSize;
  public final long firstPayloadPageGranulePosition;

  private OggTestFile(byte[] data, long lastGranule, int packetCount, int pageCount,
      int firstPayloadPageSize, long firstPayloadPageGranulePosition) {
    this.data = data;
    this.lastGranule = lastGranule;
    this.packetCount = packetCount;
    this.pageCount = pageCount;
    this.firstPayloadPageSize = firstPayloadPageSize;
    this.firstPayloadPageGranulePosition = firstPayloadPageGranulePosition;
  }

  public static OggTestFile generate(Random random, int pageCount) {
    ArrayList<byte[]> fileData = new ArrayList<>();
    int fileSize = 0;
    long granule = 0;
    int packetLength = -1;
    int packetCount = 0;
    int firstPayloadPageSize = 0;
    long firstPayloadPageGranulePosition = 0;

    for (int i = 0; i < pageCount; i++) {
      int headerType = 0x00;
      if (packetLength >= 0) {
        headerType |= 1;
      }
      if (i == 0) {
        headerType |= 2;
      }
      if (i == pageCount - 1) {
        headerType |= 4;
      }
      granule += random.nextInt(MAX_GRANULES_IN_PAGE - 1) + 1;
      int pageSegmentCount = random.nextInt(MAX_SEGMENT_COUNT);
      byte[] header = OggTestData.buildOggHeader(headerType, granule, 0, pageSegmentCount);
      fileData.add(header);
      fileSize += header.length;

      byte[] laces = new byte[pageSegmentCount];
      int bodySize = 0;
      for (int j = 0; j < pageSegmentCount; j++) {
        if (packetLength < 0) {
          packetCount++;
          if (i < pageCount - 1) {
            packetLength = random.nextInt(MAX_PACKET_LENGTH);
          } else {
            int maxPacketLength = 255 * (pageSegmentCount - j) - 1;
            packetLength = random.nextInt(maxPacketLength);
          }
        } else if (i == pageCount - 1 && j == pageSegmentCount - 1) {
          packetLength = Math.min(packetLength, 254);
        }
        laces[j] = (byte) Math.min(packetLength, 255);
        bodySize += laces[j] & 0xFF;
        packetLength -= 255;
      }
      fileData.add(laces);
      fileSize += laces.length;

      byte[] payload = TestUtil.buildTestData(bodySize, random);
      fileData.add(payload);
      fileSize += payload.length;
      if (i == 0) {
        firstPayloadPageSize = header.length + bodySize;
        firstPayloadPageGranulePosition = granule;
      }
    }

    byte[] file = new byte[fileSize];
    int position = 0;
    for (byte[] data : fileData) {
      System.arraycopy(data, 0, file, position, data.length);
      position += data.length;
    }
    return new OggTestFile(file, granule, packetCount, pageCount, firstPayloadPageSize,
        firstPayloadPageGranulePosition);
  }

  public int findPreviousPageStart(long position) {
    for (int i = (int) (position - 4); i >= 0; i--) {
      if (data[i] == 'O' && data[i + 1] == 'g' && data[i + 2] == 'g' && data[i + 3] == 'S') {
        return i;
      }
    }
    Assert.fail();
    return -1;
  }

}
