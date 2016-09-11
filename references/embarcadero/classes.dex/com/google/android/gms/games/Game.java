package com.google.android.gms.games;

import android.database.CharArrayBuffer;
import android.net.Uri;
import android.os.Parcelable;
import com.google.android.gms.common.data.Freezable;

public interface Game extends Parcelable, Freezable<Game> {
    int getAchievementTotalCount();

    String getApplicationId();

    String getDescription();

    void getDescription(CharArrayBuffer charArrayBuffer);

    String getDeveloperName();

    void getDeveloperName(CharArrayBuffer charArrayBuffer);

    String getDisplayName();

    void getDisplayName(CharArrayBuffer charArrayBuffer);

    Uri getFeaturedImageUri();

    @Deprecated
    String getFeaturedImageUrl();

    int getGameplayAclStatus();

    Uri getHiResImageUri();

    @Deprecated
    String getHiResImageUrl();

    Uri getIconImageUri();

    @Deprecated
    String getIconImageUrl();

    String getInstancePackageName();

    int getLeaderboardCount();

    String getPrimaryCategory();

    String getSecondaryCategory();

    boolean isInstanceInstalled();

    boolean isMuted();

    boolean isPlayEnabledGame();

    boolean isRealTimeMultiplayerEnabled();

    boolean isTurnBasedMultiplayerEnabled();
}
