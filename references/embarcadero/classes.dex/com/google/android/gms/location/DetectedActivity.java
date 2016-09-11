package com.google.android.gms.location;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public class DetectedActivity implements SafeParcelable {
    public static final DetectedActivityCreator CREATOR;
    public static final int IN_VEHICLE = 0;
    public static final int ON_BICYCLE = 1;
    public static final int ON_FOOT = 2;
    public static final int STILL = 3;
    public static final int TILTING = 5;
    public static final int UNKNOWN = 4;
    int KS;
    int KT;
    private final int wj;

    static {
        CREATOR = new DetectedActivityCreator();
    }

    public DetectedActivity(int activityType, int confidence) {
        this.wj = ON_BICYCLE;
        this.KS = activityType;
        this.KT = confidence;
    }

    public DetectedActivity(int versionCode, int activityType, int confidence) {
        this.wj = versionCode;
        this.KS = activityType;
        this.KT = confidence;
    }

    private int bh(int i) {
        return i > 6 ? UNKNOWN : i;
    }

    public int describeContents() {
        return IN_VEHICLE;
    }

    public int getConfidence() {
        return this.KT;
    }

    public int getType() {
        return bh(this.KS);
    }

    public int getVersionCode() {
        return this.wj;
    }

    public String toString() {
        return "DetectedActivity [type=" + getType() + ", confidence=" + this.KT + "]";
    }

    public void writeToParcel(Parcel out, int flags) {
        DetectedActivityCreator.a(this, out, flags);
    }
}
