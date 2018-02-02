/*
 * Copyright (c) 2017 The Android Open Source Project
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
package com.google.android.exoplayer2.ext.mediasession;

import android.os.Bundle;
import android.os.ResultReceiver;
import android.support.v4.media.session.PlaybackStateCompat;
import com.google.android.exoplayer2.C;
import com.google.android.exoplayer2.Player;
import com.google.android.exoplayer2.util.RepeatModeUtil;

/**
 * A default implementation of {@link MediaSessionConnector.PlaybackController}.
 * <p>
 * Methods can be safely overridden by subclasses to intercept calls for given actions.
 */
public class DefaultPlaybackController implements MediaSessionConnector.PlaybackController {

  /**
   * The default fast forward increment, in milliseconds.
   */
  public static final int DEFAULT_FAST_FORWARD_MS = 15000;
  /**
   * The default rewind increment, in milliseconds.
   */
  public static final int DEFAULT_REWIND_MS = 5000;

  private static final long BASE_ACTIONS = PlaybackStateCompat.ACTION_PLAY_PAUSE
      | PlaybackStateCompat.ACTION_PLAY | PlaybackStateCompat.ACTION_PAUSE
      | PlaybackStateCompat.ACTION_STOP | PlaybackStateCompat.ACTION_SET_SHUFFLE_MODE
      | PlaybackStateCompat.ACTION_SET_REPEAT_MODE;

  protected final long rewindIncrementMs;
  protected final long fastForwardIncrementMs;
  protected final int repeatToggleModes;

  /**
   * Creates a new instance.
   * <p>
   * Equivalent to {@code DefaultPlaybackController(DefaultPlaybackController.DEFAULT_REWIND_MS,
   *     DefaultPlaybackController.DEFAULT_FAST_FORWARD_MS,
   *     MediaSessionConnector.DEFAULT_REPEAT_TOGGLE_MODES)}.
   */
  public DefaultPlaybackController() {
    this(DEFAULT_REWIND_MS, DEFAULT_FAST_FORWARD_MS,
        MediaSessionConnector.DEFAULT_REPEAT_TOGGLE_MODES);
  }

  /**
   * Creates a new instance with the given fast forward and rewind increments.
   *  @param rewindIncrementMs The rewind increment in milliseconds. A zero or negative value will
   *     cause the rewind action to be disabled.
   * @param fastForwardIncrementMs The fast forward increment in milliseconds. A zero or negative
   * @param repeatToggleModes The available repeatToggleModes.
   */
  public DefaultPlaybackController(long rewindIncrementMs, long fastForwardIncrementMs,
      @RepeatModeUtil.RepeatToggleModes int repeatToggleModes) {
    this.rewindIncrementMs = rewindIncrementMs;
    this.fastForwardIncrementMs = fastForwardIncrementMs;
    this.repeatToggleModes = repeatToggleModes;
  }

  @Override
  public long getSupportedPlaybackActions(Player player) {
    if (player == null || player.getCurrentTimeline().isEmpty()) {
      return 0;
    }
    long actions = BASE_ACTIONS;
    if (player.isCurrentWindowSeekable()) {
      actions |= PlaybackStateCompat.ACTION_SEEK_TO;
    }
    if (fastForwardIncrementMs > 0) {
      actions |= PlaybackStateCompat.ACTION_FAST_FORWARD;
    }
    if (rewindIncrementMs > 0) {
      actions |= PlaybackStateCompat.ACTION_REWIND;
    }
    return actions;
  }

  @Override
  public void onPlay(Player player) {
    player.setPlayWhenReady(true);
  }

  @Override
  public void onPause(Player player) {
    player.setPlayWhenReady(false);
  }

  @Override
  public void onSeekTo(Player player, long position) {
    long duration = player.getDuration();
    if (duration != C.TIME_UNSET) {
      position = Math.min(position, duration);
    }
    player.seekTo(Math.max(position, 0));
  }

  @Override
  public void onFastForward(Player player) {
    if (fastForwardIncrementMs <= 0) {
      return;
    }
    onSeekTo(player, player.getCurrentPosition() + fastForwardIncrementMs);
  }

  @Override
  public void onRewind(Player player) {
    if (rewindIncrementMs <= 0) {
      return;
    }
    onSeekTo(player, player.getCurrentPosition() - rewindIncrementMs);
  }

  @Override
  public void onStop(Player player) {
    player.stop();
  }

  @Override
  public void onSetShuffleMode(Player player, int shuffleMode) {
    player.setShuffleModeEnabled(shuffleMode == PlaybackStateCompat.SHUFFLE_MODE_ALL
        || shuffleMode == PlaybackStateCompat.SHUFFLE_MODE_GROUP);
  }

  @Override
  public void onSetRepeatMode(Player player, int repeatMode) {
    int selectedExoPlayerRepeatMode = player.getRepeatMode();
    switch (repeatMode) {
      case PlaybackStateCompat.REPEAT_MODE_ALL:
      case PlaybackStateCompat.REPEAT_MODE_GROUP:
        if ((repeatToggleModes & RepeatModeUtil.REPEAT_TOGGLE_MODE_ALL) != 0) {
          selectedExoPlayerRepeatMode = Player.REPEAT_MODE_ALL;
        }
        break;
      case PlaybackStateCompat.REPEAT_MODE_ONE:
        if ((repeatToggleModes & RepeatModeUtil.REPEAT_TOGGLE_MODE_ONE) != 0) {
          selectedExoPlayerRepeatMode = Player.REPEAT_MODE_ONE;
        }
        break;
      default:
        selectedExoPlayerRepeatMode = Player.REPEAT_MODE_OFF;
        break;
    }
    player.setRepeatMode(selectedExoPlayerRepeatMode);
  }

  // CommandReceiver implementation.

  @Override
  public String[] getCommands() {
    return null;
  }

  @Override
  public void onCommand(Player player, String command, Bundle extras, ResultReceiver cb) {
    // Do nothing.
  }

}
