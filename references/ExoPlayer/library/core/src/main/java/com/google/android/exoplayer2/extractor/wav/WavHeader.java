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
package com.google.android.exoplayer2.extractor.wav;

import com.google.android.exoplayer2.C;
import com.google.android.exoplayer2.extractor.SeekMap;
import com.google.android.exoplayer2.util.Util;

/** Header for a WAV file. */
/* package */ final class WavHeader implements SeekMap {

  /** Number of audio chanels. */
  private final int numChannels;
  /** Sample rate in Hertz. */
  private final int sampleRateHz;
  /** Average bytes per second for the sample data. */
  private final int averageBytesPerSecond;
  /** Alignment for frames of audio data; should equal {@code numChannels * bitsPerSample / 8}. */
  private final int blockAlignment;
  /** Bits per sample for the audio data. */
  private final int bitsPerSample;
  /** The PCM encoding */
  @C.PcmEncoding
  private final int encoding;

  /** Offset to the start of sample data. */
  private long dataStartPosition;
  /** Total size in bytes of the sample data. */
  private long dataSize;

  public WavHeader(int numChannels, int sampleRateHz, int averageBytesPerSecond, int blockAlignment,
      int bitsPerSample, @C.PcmEncoding int encoding) {
    this.numChannels = numChannels;
    this.sampleRateHz = sampleRateHz;
    this.averageBytesPerSecond = averageBytesPerSecond;
    this.blockAlignment = blockAlignment;
    this.bitsPerSample = bitsPerSample;
    this.encoding = encoding;
  }

  // Setting bounds.

  /**
   * Sets the data start position and size in bytes of sample data in this WAV.
   *
   * @param dataStartPosition The data start position in bytes.
   * @param dataSize The data size in bytes.
   */
  public void setDataBounds(long dataStartPosition, long dataSize) {
    this.dataStartPosition = dataStartPosition;
    this.dataSize = dataSize;
  }

  /** Returns whether the data start position and size have been set. */
  public boolean hasDataBounds() {
    return dataStartPosition != 0 && dataSize != 0;
  }

  // SeekMap implementation.

  @Override
  public boolean isSeekable() {
    return true;
  }

  @Override
  public long getDurationUs() {
    long numFrames = dataSize / blockAlignment;
    return (numFrames * C.MICROS_PER_SECOND) / sampleRateHz;
  }

  @Override
  public long getPosition(long timeUs) {
    long positionOffset = (timeUs * averageBytesPerSecond) / C.MICROS_PER_SECOND;
    // Constrain to nearest preceding frame offset.
    positionOffset = (positionOffset / blockAlignment) * blockAlignment;
    positionOffset = Util.constrainValue(positionOffset, 0, dataSize - blockAlignment);
    // Add data start position.
    return dataStartPosition + positionOffset;
  }

  // Misc getters.

  /**
   * Returns the time in microseconds for the given position in bytes.
   *
   * @param position The position in bytes.
   */
  public long getTimeUs(long position) {
    return position * C.MICROS_PER_SECOND / averageBytesPerSecond;
  }

  /** Returns the bytes per frame of this WAV. */
  public int getBytesPerFrame() {
    return blockAlignment;
  }

  /** Returns the bitrate of this WAV. */
  public int getBitrate() {
    return sampleRateHz * bitsPerSample * numChannels;
  }

  /** Returns the sample rate in Hertz of this WAV. */
  public int getSampleRateHz() {
    return sampleRateHz;
  }

  /** Returns the number of audio channels in this WAV. */
  public int getNumChannels() {
    return numChannels;
  }

  /** Returns the PCM encoding. **/
  public @C.PcmEncoding int getEncoding() {
    return encoding;
  }

}
