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
package com.google.android.exoplayer2.source;

import android.net.Uri;
import android.os.Handler;
import android.support.annotation.Nullable;
import com.google.android.exoplayer2.C;
import com.google.android.exoplayer2.ExoPlayer;
import com.google.android.exoplayer2.Format;
import com.google.android.exoplayer2.Player;
import com.google.android.exoplayer2.extractor.DefaultExtractorsFactory;
import com.google.android.exoplayer2.extractor.Extractor;
import com.google.android.exoplayer2.extractor.ExtractorsFactory;
import com.google.android.exoplayer2.source.MediaSourceEventListener.EventDispatcher;
import com.google.android.exoplayer2.source.ads.AdsMediaSource;
import com.google.android.exoplayer2.upstream.Allocator;
import com.google.android.exoplayer2.upstream.DataSource;
import com.google.android.exoplayer2.upstream.DataSpec;
import com.google.android.exoplayer2.util.Assertions;
import java.io.IOException;

/**
 * Provides one period that loads data from a {@link Uri} and extracted using an {@link Extractor}.
 * <p>
 * If the possible input stream container formats are known, pass a factory that instantiates
 * extractors for them to the constructor. Otherwise, pass a {@link DefaultExtractorsFactory} to
 * use the default extractors. When reading a new stream, the first {@link Extractor} in the array
 * of extractors created by the factory that returns {@code true} from {@link Extractor#sniff} will
 * be used to extract samples from the input stream.
 * <p>
 * Note that the built-in extractors for AAC, MPEG PS/TS and FLV streams do not support seeking.
 */
public final class ExtractorMediaSource implements MediaSource, ExtractorMediaPeriod.Listener {
  /**
   * Listener of {@link ExtractorMediaSource} events.
   *
   * @deprecated Use {@link MediaSourceEventListener}.
   */
  @Deprecated
  public interface EventListener {

    /**
     * Called when an error occurs loading media data.
     * <p>
     * This method being called does not indicate that playback has failed, or that it will fail.
     * The player may be able to recover from the error and continue. Hence applications should
     * <em>not</em> implement this method to display a user visible error or initiate an application
     * level retry ({@link Player.EventListener#onPlayerError} is the appropriate place to implement
     * such behavior). This method is called to provide the application with an opportunity to log
     * the error if it wishes to do so.
     *
     * @param error The load error.
     */
    void onLoadError(IOException error);

  }

  /**
   * The default minimum number of times to retry loading prior to failing for on-demand streams.
   */
  public static final int DEFAULT_MIN_LOADABLE_RETRY_COUNT_ON_DEMAND = 3;

  /**
   * The default minimum number of times to retry loading prior to failing for live streams.
   */
  public static final int DEFAULT_MIN_LOADABLE_RETRY_COUNT_LIVE = 6;

  /**
   * Value for {@code minLoadableRetryCount} that causes the loader to retry
   * {@link #DEFAULT_MIN_LOADABLE_RETRY_COUNT_LIVE} times for live streams and
   * {@link #DEFAULT_MIN_LOADABLE_RETRY_COUNT_ON_DEMAND} for on-demand streams.
   */
  public static final int MIN_RETRY_COUNT_DEFAULT_FOR_MEDIA = -1;

  /**
   * The default number of bytes that should be loaded between each each invocation of
   * {@link MediaPeriod.Callback#onContinueLoadingRequested(SequenceableLoader)}.
   */
  public static final int DEFAULT_LOADING_CHECK_INTERVAL_BYTES = 1024 * 1024;

  private final Uri uri;
  private final DataSource.Factory dataSourceFactory;
  private final ExtractorsFactory extractorsFactory;
  private final int minLoadableRetryCount;
  private final EventDispatcher eventDispatcher;
  private final String customCacheKey;
  private final int continueLoadingCheckIntervalBytes;

  private MediaSource.Listener sourceListener;
  private long timelineDurationUs;
  private boolean timelineIsSeekable;

  /** Factory for {@link ExtractorMediaSource}s. */
  public static final class Factory implements AdsMediaSource.MediaSourceFactory {

    private final DataSource.Factory dataSourceFactory;

    private @Nullable ExtractorsFactory extractorsFactory;
    private @Nullable String customCacheKey;
    private int minLoadableRetryCount;
    private int continueLoadingCheckIntervalBytes;
    private boolean isCreateCalled;

    /**
     * Creates a new factory for {@link ExtractorMediaSource}s.
     *
     * @param dataSourceFactory A factory for {@link DataSource}s to read the media.
     */
    public Factory(DataSource.Factory dataSourceFactory) {
      this.dataSourceFactory = dataSourceFactory;
      minLoadableRetryCount = MIN_RETRY_COUNT_DEFAULT_FOR_MEDIA;
      continueLoadingCheckIntervalBytes = DEFAULT_LOADING_CHECK_INTERVAL_BYTES;
    }

    /**
     * Sets the factory for {@link Extractor}s to process the media stream. The default value is an
     * instance of {@link DefaultExtractorsFactory}.
     *
     * @param extractorsFactory A factory for {@link Extractor}s to process the media stream. If the
     *     possible formats are known, pass a factory that instantiates extractors for those
     *     formats.
     * @return This factory, for convenience.
     * @throws IllegalStateException If one of the {@code create} methods has already been called.
     */
    public Factory setExtractorsFactory(ExtractorsFactory extractorsFactory) {
      Assertions.checkState(!isCreateCalled);
      this.extractorsFactory = extractorsFactory;
      return this;
    }

    /**
     * Sets the custom key that uniquely identifies the original stream. Used for cache indexing.
     * The default value is {@code null}.
     *
     * @param customCacheKey A custom key that uniquely identifies the original stream. Used for
     *     cache indexing.
     * @return This factory, for convenience.
     * @throws IllegalStateException If one of the {@code create} methods has already been called.
     */
    public Factory setCustomCacheKey(String customCacheKey) {
      Assertions.checkState(!isCreateCalled);
      this.customCacheKey = customCacheKey;
      return this;
    }

    /**
     * Sets the minimum number of times to retry if a loading error occurs. The default value is
     * {@link #MIN_RETRY_COUNT_DEFAULT_FOR_MEDIA}.
     *
     * @param minLoadableRetryCount The minimum number of times to retry if a loading error occurs.
     * @return This factory, for convenience.
     * @throws IllegalStateException If one of the {@code create} methods has already been called.
     */
    public Factory setMinLoadableRetryCount(int minLoadableRetryCount) {
      Assertions.checkState(!isCreateCalled);
      this.minLoadableRetryCount = minLoadableRetryCount;
      return this;
    }

    /**
     * Sets the number of bytes that should be loaded between each invocation of {@link
     * MediaPeriod.Callback#onContinueLoadingRequested(SequenceableLoader)}. The default value is
     * {@link #DEFAULT_LOADING_CHECK_INTERVAL_BYTES}.
     *
     * @param continueLoadingCheckIntervalBytes The number of bytes that should be loaded between
     *     each invocation of {@link
     *     MediaPeriod.Callback#onContinueLoadingRequested(SequenceableLoader)}.
     * @return This factory, for convenience.
     * @throws IllegalStateException If one of the {@code create} methods has already been called.
     */
    public Factory setContinueLoadingCheckIntervalBytes(int continueLoadingCheckIntervalBytes) {
      Assertions.checkState(!isCreateCalled);
      this.continueLoadingCheckIntervalBytes = continueLoadingCheckIntervalBytes;
      return this;
    }

    /**
     * Returns a new {@link ExtractorMediaSource} using the current parameters. Media source events
     * will not be delivered.
     *
     * @param uri The {@link Uri}.
     * @return The new {@link ExtractorMediaSource}.
     */
    public ExtractorMediaSource createMediaSource(Uri uri) {
      return createMediaSource(uri, null, null);
    }

    /**
     * Returns a new {@link ExtractorMediaSource} using the current parameters.
     *
     * @param uri The {@link Uri}.
     * @param eventHandler A handler for events.
     * @param eventListener A listener of events.
     * @return The new {@link ExtractorMediaSource}.
     */
    @Override
    public ExtractorMediaSource createMediaSource(
        Uri uri, @Nullable Handler eventHandler, @Nullable MediaSourceEventListener eventListener) {
      isCreateCalled = true;
      if (extractorsFactory == null) {
        extractorsFactory = new DefaultExtractorsFactory();
      }
      return new ExtractorMediaSource(uri, dataSourceFactory, extractorsFactory,
          minLoadableRetryCount, eventHandler, eventListener, customCacheKey,
          continueLoadingCheckIntervalBytes);
    }

    @Override
    public int[] getSupportedTypes() {
      return new int[] {C.TYPE_OTHER};
    }
  }

  /**
   * @param uri The {@link Uri} of the media stream.
   * @param dataSourceFactory A factory for {@link DataSource}s to read the media.
   * @param extractorsFactory A factory for {@link Extractor}s to process the media stream. If the
   *     possible formats are known, pass a factory that instantiates extractors for those formats.
   *     Otherwise, pass a {@link DefaultExtractorsFactory} to use default extractors.
   * @param eventHandler A handler for events. May be null if delivery of events is not required.
   * @param eventListener A listener of events. May be null if delivery of events is not required.
   * @deprecated Use {@link Factory} instead.
   */
  @Deprecated
  public ExtractorMediaSource(
      Uri uri,
      DataSource.Factory dataSourceFactory,
      ExtractorsFactory extractorsFactory,
      Handler eventHandler,
      EventListener eventListener) {
    this(uri, dataSourceFactory, extractorsFactory, eventHandler, eventListener, null);
  }

  /**
   * @param uri The {@link Uri} of the media stream.
   * @param dataSourceFactory A factory for {@link DataSource}s to read the media.
   * @param extractorsFactory A factory for {@link Extractor}s to process the media stream. If the
   *     possible formats are known, pass a factory that instantiates extractors for those formats.
   *     Otherwise, pass a {@link DefaultExtractorsFactory} to use default extractors.
   * @param eventHandler A handler for events. May be null if delivery of events is not required.
   * @param eventListener A listener of events. May be null if delivery of events is not required.
   * @param customCacheKey A custom key that uniquely identifies the original stream. Used for cache
   *     indexing. May be null.
   * @deprecated Use {@link Factory} instead.
   */
  @Deprecated
  public ExtractorMediaSource(
      Uri uri,
      DataSource.Factory dataSourceFactory,
      ExtractorsFactory extractorsFactory,
      Handler eventHandler,
      EventListener eventListener,
      String customCacheKey) {
    this(uri, dataSourceFactory, extractorsFactory, MIN_RETRY_COUNT_DEFAULT_FOR_MEDIA, eventHandler,
        eventListener, customCacheKey, DEFAULT_LOADING_CHECK_INTERVAL_BYTES);
  }

  /**
   * @param uri The {@link Uri} of the media stream.
   * @param dataSourceFactory A factory for {@link DataSource}s to read the media.
   * @param extractorsFactory A factory for {@link Extractor}s to process the media stream. If the
   *     possible formats are known, pass a factory that instantiates extractors for those formats.
   *     Otherwise, pass a {@link DefaultExtractorsFactory} to use default extractors.
   * @param minLoadableRetryCount The minimum number of times to retry if a loading error occurs.
   * @param eventHandler A handler for events. May be null if delivery of events is not required.
   * @param eventListener A listener of events. May be null if delivery of events is not required.
   * @param customCacheKey A custom key that uniquely identifies the original stream. Used for cache
   *     indexing. May be null.
   * @param continueLoadingCheckIntervalBytes The number of bytes that should be loaded between each
   *     invocation of {@link MediaPeriod.Callback#onContinueLoadingRequested(SequenceableLoader)}.
   * @deprecated Use {@link Factory} instead.
   */
  @Deprecated
  public ExtractorMediaSource(
      Uri uri,
      DataSource.Factory dataSourceFactory,
      ExtractorsFactory extractorsFactory,
      int minLoadableRetryCount,
      Handler eventHandler,
      EventListener eventListener,
      String customCacheKey,
      int continueLoadingCheckIntervalBytes) {
    this(
        uri,
        dataSourceFactory,
        extractorsFactory,
        minLoadableRetryCount,
        eventHandler,
        eventListener == null ? null : new EventListenerWrapper(eventListener),
        customCacheKey,
        continueLoadingCheckIntervalBytes);
  }

  private ExtractorMediaSource(
      Uri uri,
      DataSource.Factory dataSourceFactory,
      ExtractorsFactory extractorsFactory,
      int minLoadableRetryCount,
      @Nullable Handler eventHandler,
      @Nullable MediaSourceEventListener eventListener,
      @Nullable String customCacheKey,
      int continueLoadingCheckIntervalBytes) {
    this.uri = uri;
    this.dataSourceFactory = dataSourceFactory;
    this.extractorsFactory = extractorsFactory;
    this.minLoadableRetryCount = minLoadableRetryCount;
    this.eventDispatcher = new EventDispatcher(eventHandler, eventListener);
    this.customCacheKey = customCacheKey;
    this.continueLoadingCheckIntervalBytes = continueLoadingCheckIntervalBytes;
  }

  @Override
  public void prepareSource(ExoPlayer player, boolean isTopLevelSource, Listener listener) {
    sourceListener = listener;
    notifySourceInfoRefreshed(C.TIME_UNSET, false);
  }

  @Override
  public void maybeThrowSourceInfoRefreshError() throws IOException {
    // Do nothing.
  }

  @Override
  public MediaPeriod createPeriod(MediaPeriodId id, Allocator allocator) {
    Assertions.checkArgument(id.periodIndex == 0);
    return new ExtractorMediaPeriod(
        uri,
        dataSourceFactory.createDataSource(),
        extractorsFactory.createExtractors(),
        minLoadableRetryCount,
        eventDispatcher,
        this,
        allocator,
        customCacheKey,
        continueLoadingCheckIntervalBytes);
  }

  @Override
  public void releasePeriod(MediaPeriod mediaPeriod) {
    ((ExtractorMediaPeriod) mediaPeriod).release();
  }

  @Override
  public void releaseSource() {
    sourceListener = null;
  }

  // ExtractorMediaPeriod.Listener implementation.

  @Override
  public void onSourceInfoRefreshed(long durationUs, boolean isSeekable) {
    // If we already have the duration from a previous source info refresh, use it.
    durationUs = durationUs == C.TIME_UNSET ? timelineDurationUs : durationUs;
    if (timelineDurationUs == durationUs && timelineIsSeekable == isSeekable) {
      // Suppress no-op source info changes.
      return;
    }
    notifySourceInfoRefreshed(durationUs, isSeekable);
  }

  // Internal methods.

  private void notifySourceInfoRefreshed(long durationUs, boolean isSeekable) {
    timelineDurationUs = durationUs;
    timelineIsSeekable = isSeekable;
    sourceListener.onSourceInfoRefreshed(
        this, new SinglePeriodTimeline(timelineDurationUs, timelineIsSeekable), null);
  }

  /**
   * Wraps a deprecated {@link EventListener}, invoking its callback from the equivalent callback in
   * {@link MediaSourceEventListener}.
   */
  private static final class EventListenerWrapper implements MediaSourceEventListener {
    private final EventListener eventListener;

    public EventListenerWrapper(EventListener eventListener) {
      this.eventListener = Assertions.checkNotNull(eventListener);
    }

    @Override
    public void onLoadStarted(
        DataSpec dataSpec,
        int dataType,
        int trackType,
        Format trackFormat,
        int trackSelectionReason,
        Object trackSelectionData,
        long mediaStartTimeMs,
        long mediaEndTimeMs,
        long elapsedRealtimeMs) {
      // Do nothing.
    }

    @Override
    public void onLoadCompleted(
        DataSpec dataSpec,
        int dataType,
        int trackType,
        Format trackFormat,
        int trackSelectionReason,
        Object trackSelectionData,
        long mediaStartTimeMs,
        long mediaEndTimeMs,
        long elapsedRealtimeMs,
        long loadDurationMs,
        long bytesLoaded) {
      // Do nothing.
    }

    @Override
    public void onLoadCanceled(
        DataSpec dataSpec,
        int dataType,
        int trackType,
        Format trackFormat,
        int trackSelectionReason,
        Object trackSelectionData,
        long mediaStartTimeMs,
        long mediaEndTimeMs,
        long elapsedRealtimeMs,
        long loadDurationMs,
        long bytesLoaded) {
      // Do nothing.
    }

    @Override
    public void onLoadError(
        DataSpec dataSpec,
        int dataType,
        int trackType,
        Format trackFormat,
        int trackSelectionReason,
        Object trackSelectionData,
        long mediaStartTimeMs,
        long mediaEndTimeMs,
        long elapsedRealtimeMs,
        long loadDurationMs,
        long bytesLoaded,
        IOException error,
        boolean wasCanceled) {
      eventListener.onLoadError(error);
    }

    @Override
    public void onUpstreamDiscarded(int trackType, long mediaStartTimeMs, long mediaEndTimeMs) {
      // Do nothing.
    }

    @Override
    public void onDownstreamFormatChanged(
        int trackType,
        Format trackFormat,
        int trackSelectionReason,
        Object trackSelectionData,
        long mediaTimeMs) {
      // Do nothing.
    }
  }
}
