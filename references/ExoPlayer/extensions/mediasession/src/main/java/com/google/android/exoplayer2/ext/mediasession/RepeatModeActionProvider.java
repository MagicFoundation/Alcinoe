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

import android.content.Context;
import android.os.Bundle;
import android.support.v4.media.session.PlaybackStateCompat;
import com.google.android.exoplayer2.Player;
import com.google.android.exoplayer2.util.RepeatModeUtil;

/**
 * Provides a custom action for toggling repeat modes.
 */
public final class RepeatModeActionProvider implements MediaSessionConnector.CustomActionProvider {

  private static final String ACTION_REPEAT_MODE = "ACTION_EXO_REPEAT_MODE";

  private final Player player;
  @RepeatModeUtil.RepeatToggleModes
  private final int repeatToggleModes;
  private final CharSequence repeatAllDescription;
  private final CharSequence repeatOneDescription;
  private final CharSequence repeatOffDescription;

  /**
   * Creates a new instance.
   * <p>
   * Equivalent to {@code RepeatModeActionProvider(context, player,
   *     MediaSessionConnector.DEFAULT_REPEAT_TOGGLE_MODES)}.
   *
   * @param context The context.
   * @param player The player on which to toggle the repeat mode.
   */
  public RepeatModeActionProvider(Context context, Player player) {
    this(context, player, MediaSessionConnector.DEFAULT_REPEAT_TOGGLE_MODES);
  }

  /**
   * Creates a new instance enabling the given repeat toggle modes.
   *
   * @param context The context.
   * @param player The player on which to toggle the repeat mode.
   * @param repeatToggleModes The toggle modes to enable.
   */
  public RepeatModeActionProvider(Context context, Player player,
      @RepeatModeUtil.RepeatToggleModes int repeatToggleModes) {
    this.player = player;
    this.repeatToggleModes = repeatToggleModes;
    repeatAllDescription = context.getString(R.string.exo_media_action_repeat_all_description);
    repeatOneDescription = context.getString(R.string.exo_media_action_repeat_one_description);
    repeatOffDescription = context.getString(R.string.exo_media_action_repeat_off_description);
  }

  @Override
  public void onCustomAction(String action, Bundle extras) {
    int mode = player.getRepeatMode();
    int proposedMode = RepeatModeUtil.getNextRepeatMode(mode, repeatToggleModes);
    if (mode != proposedMode) {
      player.setRepeatMode(proposedMode);
    }
  }

  @Override
  public PlaybackStateCompat.CustomAction getCustomAction() {
    CharSequence actionLabel;
    int iconResourceId;
    switch (player.getRepeatMode()) {
      case Player.REPEAT_MODE_ONE:
        actionLabel = repeatOneDescription;
        iconResourceId = R.drawable.exo_media_action_repeat_one;
        break;
      case Player.REPEAT_MODE_ALL:
        actionLabel = repeatAllDescription;
        iconResourceId = R.drawable.exo_media_action_repeat_all;
        break;
      case Player.REPEAT_MODE_OFF:
      default:
        actionLabel = repeatOffDescription;
        iconResourceId = R.drawable.exo_media_action_repeat_off;
        break;
    }
    PlaybackStateCompat.CustomAction.Builder repeatBuilder = new PlaybackStateCompat.CustomAction
        .Builder(ACTION_REPEAT_MODE, actionLabel, iconResourceId);
    return repeatBuilder.build();
  }

}
