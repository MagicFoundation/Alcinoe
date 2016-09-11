package com.google.android.gms.games.achievement;

import android.content.Intent;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Releasable;
import com.google.android.gms.common.api.Result;

public interface Achievements {

    public interface LoadAchievementsResult extends Releasable, Result {
        AchievementBuffer getAchievements();
    }

    public interface UpdateAchievementResult extends Result {
        String getAchievementId();
    }

    Intent getAchievementsIntent(GoogleApiClient googleApiClient);

    void increment(GoogleApiClient googleApiClient, String str, int i);

    PendingResult<UpdateAchievementResult> incrementImmediate(GoogleApiClient googleApiClient, String str, int i);

    PendingResult<LoadAchievementsResult> load(GoogleApiClient googleApiClient, boolean z);

    void reveal(GoogleApiClient googleApiClient, String str);

    PendingResult<UpdateAchievementResult> revealImmediate(GoogleApiClient googleApiClient, String str);

    void setSteps(GoogleApiClient googleApiClient, String str, int i);

    PendingResult<UpdateAchievementResult> setStepsImmediate(GoogleApiClient googleApiClient, String str, int i);

    void unlock(GoogleApiClient googleApiClient, String str);

    PendingResult<UpdateAchievementResult> unlockImmediate(GoogleApiClient googleApiClient, String str);
}
