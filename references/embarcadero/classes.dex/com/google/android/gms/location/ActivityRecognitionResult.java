package com.google.android.gms.location;

import android.content.Intent;
import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.internal.er;
import java.util.Collections;
import java.util.List;

public class ActivityRecognitionResult implements SafeParcelable {
    public static final ActivityRecognitionResultCreator CREATOR;
    public static final String EXTRA_ACTIVITY_RESULT = "com.google.android.location.internal.EXTRA_ACTIVITY_RESULT";
    List<DetectedActivity> KP;
    long KQ;
    long KR;
    private final int wj;

    static {
        CREATOR = new ActivityRecognitionResultCreator();
    }

    public ActivityRecognitionResult(int versionCode, List<DetectedActivity> probableActivities, long timeMillis, long elapsedRealtimeMillis) {
        this.wj = 1;
        this.KP = probableActivities;
        this.KQ = timeMillis;
        this.KR = elapsedRealtimeMillis;
    }

    public ActivityRecognitionResult(DetectedActivity mostProbableActivity, long time, long elapsedRealtimeMillis) {
        this(Collections.singletonList(mostProbableActivity), time, elapsedRealtimeMillis);
    }

    public ActivityRecognitionResult(List<DetectedActivity> probableActivities, long time, long elapsedRealtimeMillis) {
        boolean z = probableActivities != null && probableActivities.size() > 0;
        er.b(z, (Object) "Must have at least 1 detected activity");
        this.wj = 1;
        this.KP = probableActivities;
        this.KQ = time;
        this.KR = elapsedRealtimeMillis;
    }

    public static ActivityRecognitionResult extractResult(Intent intent) {
        return !hasResult(intent) ? null : (ActivityRecognitionResult) intent.getExtras().get(EXTRA_ACTIVITY_RESULT);
    }

    public static boolean hasResult(Intent intent) {
        return intent == null ? false : intent.hasExtra(EXTRA_ACTIVITY_RESULT);
    }

    public int describeContents() {
        return 0;
    }

    public int getActivityConfidence(int activityType) {
        for (DetectedActivity detectedActivity : this.KP) {
            if (detectedActivity.getType() == activityType) {
                return detectedActivity.getConfidence();
            }
        }
        return 0;
    }

    public long getElapsedRealtimeMillis() {
        return this.KR;
    }

    public DetectedActivity getMostProbableActivity() {
        return (DetectedActivity) this.KP.get(0);
    }

    public List<DetectedActivity> getProbableActivities() {
        return this.KP;
    }

    public long getTime() {
        return this.KQ;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public String toString() {
        return "ActivityRecognitionResult [probableActivities=" + this.KP + ", timeMillis=" + this.KQ + ", elapsedRealtimeMillis=" + this.KR + "]";
    }

    public void writeToParcel(Parcel out, int flags) {
        ActivityRecognitionResultCreator.a(this, out, flags);
    }
}
