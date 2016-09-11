package com.google.android.gms.games.achievement;

import android.database.CharArrayBuffer;
import android.net.Uri;
import com.google.android.gms.games.Player;

public interface Achievement {
    public static final int STATE_HIDDEN = 2;
    public static final int STATE_REVEALED = 1;
    public static final int STATE_UNLOCKED = 0;
    public static final int TYPE_INCREMENTAL = 1;
    public static final int TYPE_STANDARD = 0;

    String getAchievementId();

    int getCurrentSteps();

    String getDescription();

    void getDescription(CharArrayBuffer charArrayBuffer);

    String getFormattedCurrentSteps();

    void getFormattedCurrentSteps(CharArrayBuffer charArrayBuffer);

    String getFormattedTotalSteps();

    void getFormattedTotalSteps(CharArrayBuffer charArrayBuffer);

    long getLastUpdatedTimestamp();

    String getName();

    void getName(CharArrayBuffer charArrayBuffer);

    Player getPlayer();

    Uri getRevealedImageUri();

    int getState();

    int getTotalSteps();

    int getType();

    Uri getUnlockedImageUri();
}
