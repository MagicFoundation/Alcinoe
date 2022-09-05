/*
 * Copyright (C) 2017 The Android Open Source Project
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
package com.google.android.exoplayer2.testutil;

import android.app.Instrumentation;
import com.google.android.exoplayer2.C;
import com.google.android.exoplayer2.extractor.Extractor;
import com.google.android.exoplayer2.extractor.ExtractorInput;
import com.google.android.exoplayer2.extractor.ExtractorOutput;
import com.google.android.exoplayer2.extractor.PositionHolder;
import com.google.android.exoplayer2.extractor.SeekMap;
import com.google.android.exoplayer2.testutil.FakeExtractorInput.SimulatedIOException;
import com.google.android.exoplayer2.util.Assertions;
import java.io.IOException;
import java.util.Arrays;
import junit.framework.Assert;

/**
 * Assertion methods for {@link Extractor}.
 */
public final class ExtractorAsserts {

  /**
   * A factory for {@link Extractor} instances.
   */
  public interface ExtractorFactory {
    Extractor create();
  }

  private static final String DUMP_EXTENSION = ".dump";
  private static final String UNKNOWN_LENGTH_EXTENSION = ".unklen" + DUMP_EXTENSION;

  /**
   * Asserts that an extractor behaves correctly given valid input data:
   * <ul>
   *   <li>Calls {@link Extractor#seek(long, long)} and {@link Extractor#release()} without calling
   *   {@link Extractor#init(ExtractorOutput)} to check these calls do not fail.</li>
   *   <li>Calls {@link #assertOutput(Extractor, String, byte[], Instrumentation, boolean, boolean,
   *   boolean, boolean)} with all possible combinations of "simulate" parameters.</li>
   * </ul>
   *
   * @param factory An {@link ExtractorFactory} which creates instances of the {@link Extractor}
   *     class which is to be tested.
   * @param file The path to the input sample.
   * @param instrumentation To be used to load the sample file.
   * @throws IOException If reading from the input fails.
   * @throws InterruptedException If interrupted while reading from the input.
   */
  public static void assertBehavior(ExtractorFactory factory, String file,
      Instrumentation instrumentation) throws IOException, InterruptedException {
    // Check behavior prior to initialization.
    Extractor extractor = factory.create();
    extractor.seek(0, 0);
    extractor.release();
    // Assert output.
    byte[] fileData = TestUtil.getByteArray(instrumentation, file);
    assertOutput(factory, file, fileData, instrumentation);
  }

  /**
   * Calls {@link #assertOutput(Extractor, String, byte[], Instrumentation, boolean, boolean,
   * boolean, boolean)} with all possible combinations of "simulate" parameters with
   * {@code sniffFirst} set to true, and makes one additional call with the "simulate" and
   * {@code sniffFirst} parameters all set to false.
   *
   * @param factory An {@link ExtractorFactory} which creates instances of the {@link Extractor}
   *     class which is to be tested.
   * @param file The path to the input sample.
   * @param data Content of the input file.
   * @param instrumentation To be used to load the sample file.
   * @throws IOException If reading from the input fails.
   * @throws InterruptedException If interrupted while reading from the input.
   */
  public static void assertOutput(ExtractorFactory factory, String file, byte[] data,
      Instrumentation instrumentation) throws IOException, InterruptedException {
    assertOutput(factory.create(), file, data, instrumentation,  true, false, false, false);
    assertOutput(factory.create(), file, data, instrumentation,  true, false, false,  true);
    assertOutput(factory.create(), file, data, instrumentation,  true, false,  true, false);
    assertOutput(factory.create(), file, data, instrumentation,  true, false,  true,  true);
    assertOutput(factory.create(), file, data, instrumentation,  true,  true, false, false);
    assertOutput(factory.create(), file, data, instrumentation,  true,  true, false,  true);
    assertOutput(factory.create(), file, data, instrumentation,  true,  true,  true, false);
    assertOutput(factory.create(), file, data, instrumentation,  true,  true,  true,  true);
    assertOutput(factory.create(), file, data, instrumentation, false, false, false, false);
  }

  /**
   * Asserts that {@code extractor} consumes {@code sampleFile} successfully and its output equals
   * to a prerecorded output dump file with the name {@code sampleFile} + "{@value
   * #DUMP_EXTENSION}". If {@code simulateUnknownLength} is true and {@code sampleFile} + "{@value
   * #UNKNOWN_LENGTH_EXTENSION}" exists, it's preferred.
   *
   * @param extractor The {@link Extractor} to be tested.
   * @param file The path to the input sample.
   * @param data Content of the input file.
   * @param instrumentation To be used to load the sample file.
   * @param sniffFirst Whether to sniff the data by calling {@link Extractor#sniff(ExtractorInput)}
   *     prior to consuming it.
   * @param simulateIOErrors Whether to simulate IO errors.
   * @param simulateUnknownLength Whether to simulate unknown input length.
   * @param simulatePartialReads Whether to simulate partial reads.
   * @return The {@link FakeExtractorOutput} used in the test.
   * @throws IOException If reading from the input fails.
   * @throws InterruptedException If interrupted while reading from the input.
   */
  public static FakeExtractorOutput assertOutput(Extractor extractor, String file, byte[] data,
      Instrumentation instrumentation, boolean sniffFirst, boolean simulateIOErrors,
      boolean simulateUnknownLength, boolean simulatePartialReads) throws IOException,
      InterruptedException {
    FakeExtractorInput input = new FakeExtractorInput.Builder().setData(data)
        .setSimulateIOErrors(simulateIOErrors)
        .setSimulateUnknownLength(simulateUnknownLength)
        .setSimulatePartialReads(simulatePartialReads).build();

    if (sniffFirst) {
      Assert.assertTrue(TestUtil.sniffTestData(extractor, input));
      input.resetPeekPosition();
    }

    FakeExtractorOutput extractorOutput = consumeTestData(extractor, input, 0, true);
    if (simulateUnknownLength
        && assetExists(instrumentation, file + UNKNOWN_LENGTH_EXTENSION)) {
      extractorOutput.assertOutput(instrumentation, file + UNKNOWN_LENGTH_EXTENSION);
    } else {
      extractorOutput.assertOutput(instrumentation, file + ".0" + DUMP_EXTENSION);
    }

    SeekMap seekMap = extractorOutput.seekMap;
    if (seekMap.isSeekable()) {
      long durationUs = seekMap.getDurationUs();
      for (int j = 0; j < 4; j++) {
        long timeUs = (durationUs * j) / 3;
        long position = seekMap.getPosition(timeUs);
        input.setPosition((int) position);
        for (int i = 0; i < extractorOutput.numberOfTracks; i++) {
          extractorOutput.trackOutputs.valueAt(i).clear();
        }

        consumeTestData(extractor, input, timeUs, extractorOutput, false);
        extractorOutput.assertOutput(instrumentation, file + '.' + j + DUMP_EXTENSION);
      }
    }

    return extractorOutput;
  }

  /**
   * Calls {@link #assertThrows(Extractor, byte[], Class, boolean, boolean, boolean)} with all
   * possible combinations of "simulate" parameters.
   *
   * @param factory An {@link ExtractorFactory} which creates instances of the {@link Extractor}
   *     class which is to be tested.
   * @param sampleFile The path to the input sample.
   * @param instrumentation To be used to load the sample file.
   * @param expectedThrowable Expected {@link Throwable} class.
   * @throws IOException If reading from the input fails.
   * @throws InterruptedException If interrupted while reading from the input.
   * @see #assertThrows(Extractor, byte[], Class, boolean, boolean, boolean)
   */
  public static void assertThrows(ExtractorFactory factory, String sampleFile,
      Instrumentation instrumentation, Class<? extends Throwable> expectedThrowable)
      throws IOException, InterruptedException {
    byte[] fileData = TestUtil.getByteArray(instrumentation, sampleFile);
    assertThrows(factory, fileData, expectedThrowable);
  }

  /**
   * Calls {@link #assertThrows(Extractor, byte[], Class, boolean, boolean, boolean)} with all
   * possible combinations of "simulate" parameters.
   *
   * @param factory An {@link ExtractorFactory} which creates instances of the {@link Extractor}
   *     class which is to be tested.
   * @param fileData Content of the input file.
   * @param expectedThrowable Expected {@link Throwable} class.
   * @throws IOException If reading from the input fails.
   * @throws InterruptedException If interrupted while reading from the input.
   * @see #assertThrows(Extractor, byte[], Class, boolean, boolean, boolean)
   */
  public static void assertThrows(ExtractorFactory factory, byte[] fileData,
      Class<? extends Throwable> expectedThrowable) throws IOException, InterruptedException {
    assertThrows(factory.create(), fileData, expectedThrowable, false, false, false);
    assertThrows(factory.create(), fileData, expectedThrowable,  true, false, false);
    assertThrows(factory.create(), fileData, expectedThrowable, false,  true, false);
    assertThrows(factory.create(), fileData, expectedThrowable,  true,  true, false);
    assertThrows(factory.create(), fileData, expectedThrowable, false, false,  true);
    assertThrows(factory.create(), fileData, expectedThrowable,  true, false,  true);
    assertThrows(factory.create(), fileData, expectedThrowable, false,  true,  true);
    assertThrows(factory.create(), fileData, expectedThrowable,  true,  true,  true);
  }

  /**
   * Asserts {@code extractor} throws {@code expectedThrowable} while consuming {@code sampleFile}.
   *
   * @param extractor The {@link Extractor} to be tested.
   * @param fileData Content of the input file.
   * @param expectedThrowable Expected {@link Throwable} class.
   * @param simulateIOErrors If true simulates IOErrors.
   * @param simulateUnknownLength If true simulates unknown input length.
   * @param simulatePartialReads If true simulates partial reads.
   * @throws IOException If reading from the input fails.
   * @throws InterruptedException If interrupted while reading from the input.
   */
  public static void assertThrows(Extractor extractor, byte[] fileData,
      Class<? extends Throwable> expectedThrowable, boolean simulateIOErrors,
      boolean simulateUnknownLength, boolean simulatePartialReads) throws IOException,
      InterruptedException {
    FakeExtractorInput input = new FakeExtractorInput.Builder().setData(fileData)
        .setSimulateIOErrors(simulateIOErrors)
        .setSimulateUnknownLength(simulateUnknownLength)
        .setSimulatePartialReads(simulatePartialReads).build();
    try {
      consumeTestData(extractor, input, 0, true);
      throw new AssertionError(expectedThrowable.getSimpleName() + " expected but not thrown");
    } catch (Throwable throwable) {
      if (expectedThrowable.equals(throwable.getClass())) {
        return; // Pass!
      }
      throw throwable;
    }
  }

  private ExtractorAsserts() {}

  private static FakeExtractorOutput consumeTestData(Extractor extractor, FakeExtractorInput input,
      long timeUs, boolean retryFromStartIfLive) throws IOException, InterruptedException {
    FakeExtractorOutput output = new FakeExtractorOutput();
    extractor.init(output);
    consumeTestData(extractor, input, timeUs, output, retryFromStartIfLive);
    return output;
  }

  private static void consumeTestData(Extractor extractor, FakeExtractorInput input, long timeUs,
      FakeExtractorOutput output, boolean retryFromStartIfLive)
      throws IOException, InterruptedException {
    extractor.seek(input.getPosition(), timeUs);
    PositionHolder seekPositionHolder = new PositionHolder();
    int readResult = Extractor.RESULT_CONTINUE;
    while (readResult != Extractor.RESULT_END_OF_INPUT) {
      try {
        // Extractor.read should not read seekPositionHolder.position. Set it to a value that's
        // likely to cause test failure if a read does occur.
        seekPositionHolder.position = Long.MIN_VALUE;
        readResult = extractor.read(input, seekPositionHolder);
        if (readResult == Extractor.RESULT_SEEK) {
          long seekPosition = seekPositionHolder.position;
          Assertions.checkState(0 <= seekPosition && seekPosition <= Integer.MAX_VALUE);
          input.setPosition((int) seekPosition);
        }
      } catch (SimulatedIOException e) {
        if (!retryFromStartIfLive) {
          continue;
        }
        boolean isOnDemand = input.getLength() != C.LENGTH_UNSET
            || (output.seekMap != null && output.seekMap.getDurationUs() != C.TIME_UNSET);
        if (isOnDemand) {
          continue;
        }
        input.setPosition(0);
        for (int i = 0; i < output.numberOfTracks; i++) {
          output.trackOutputs.valueAt(i).clear();
        }
        extractor.seek(0, 0);
      }
    }
  }

  private static boolean assetExists(Instrumentation instrumentation, String fileName)
      throws IOException {
    int i = fileName.lastIndexOf('/');
    String path = i >= 0 ? fileName.substring(0, i) : "";
    String file = i >= 0 ? fileName.substring(i + 1) : fileName;
    return Arrays.asList(instrumentation.getContext().getResources().getAssets().list(path))
        .contains(file);
  }

}
