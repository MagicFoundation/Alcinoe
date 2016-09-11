package com.google.android.gms.location;

import android.os.Parcel;
import android.os.SystemClock;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.internal.ep;

public final class LocationRequest implements SafeParcelable {
    public static final LocationRequestCreator CREATOR;
    public static final int PRIORITY_BALANCED_POWER_ACCURACY = 102;
    public static final int PRIORITY_HIGH_ACCURACY = 100;
    public static final int PRIORITY_LOW_POWER = 104;
    public static final int PRIORITY_NO_POWER = 105;
    long KV;
    long Lc;
    long Ld;
    boolean Le;
    int Lf;
    float Lg;
    int mPriority;
    private final int wj;

    static {
        CREATOR = new LocationRequestCreator();
    }

    public LocationRequest() {
        this.wj = 1;
        this.mPriority = PRIORITY_BALANCED_POWER_ACCURACY;
        this.Lc = 3600000;
        this.Ld = 600000;
        this.Le = false;
        this.KV = Long.MAX_VALUE;
        this.Lf = Integer.MAX_VALUE;
        this.Lg = 0.0f;
    }

    LocationRequest(int versionCode, int priority, long interval, long fastestInterval, boolean explicitFastestInterval, long expireAt, int numUpdates, float smallestDisplacement) {
        this.wj = versionCode;
        this.mPriority = priority;
        this.Lc = interval;
        this.Ld = fastestInterval;
        this.Le = explicitFastestInterval;
        this.KV = expireAt;
        this.Lf = numUpdates;
        this.Lg = smallestDisplacement;
    }

    private static void a(float f) {
        if (f < 0.0f) {
            throw new IllegalArgumentException("invalid displacement: " + f);
        }
    }

    private static void bi(int i) {
        switch (i) {
            case PRIORITY_HIGH_ACCURACY /*100*/:
            case PRIORITY_BALANCED_POWER_ACCURACY /*102*/:
            case PRIORITY_LOW_POWER /*104*/:
            case PRIORITY_NO_POWER /*105*/:
            default:
                throw new IllegalArgumentException("invalid quality: " + i);
        }
    }

    public static String bj(int i) {
        switch (i) {
            case PRIORITY_HIGH_ACCURACY /*100*/:
                return "PRIORITY_HIGH_ACCURACY";
            case PRIORITY_BALANCED_POWER_ACCURACY /*102*/:
                return "PRIORITY_BALANCED_POWER_ACCURACY";
            case PRIORITY_LOW_POWER /*104*/:
                return "PRIORITY_LOW_POWER";
            case PRIORITY_NO_POWER /*105*/:
                return "PRIORITY_NO_POWER";
            default:
                return "???";
        }
    }

    public static LocationRequest create() {
        return new LocationRequest();
    }

    private static void r(long j) {
        if (j < 0) {
            throw new IllegalArgumentException("invalid interval: " + j);
        }
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object object) {
        if (this == object) {
            return true;
        }
        if (!(object instanceof LocationRequest)) {
            return false;
        }
        LocationRequest locationRequest = (LocationRequest) object;
        return this.mPriority == locationRequest.mPriority && this.Lc == locationRequest.Lc && this.Ld == locationRequest.Ld && this.Le == locationRequest.Le && this.KV == locationRequest.KV && this.Lf == locationRequest.Lf && this.Lg == locationRequest.Lg;
    }

    public long getExpirationTime() {
        return this.KV;
    }

    public long getFastestInterval() {
        return this.Ld;
    }

    public long getInterval() {
        return this.Lc;
    }

    public int getNumUpdates() {
        return this.Lf;
    }

    public int getPriority() {
        return this.mPriority;
    }

    public float getSmallestDisplacement() {
        return this.Lg;
    }

    int getVersionCode() {
        return this.wj;
    }

    public int hashCode() {
        return ep.hashCode(Integer.valueOf(this.mPriority), Long.valueOf(this.Lc), Long.valueOf(this.Ld), Boolean.valueOf(this.Le), Long.valueOf(this.KV), Integer.valueOf(this.Lf), Float.valueOf(this.Lg));
    }

    public LocationRequest setExpirationDuration(long millis) {
        long elapsedRealtime = SystemClock.elapsedRealtime();
        if (millis > Long.MAX_VALUE - elapsedRealtime) {
            this.KV = Long.MAX_VALUE;
        } else {
            this.KV = elapsedRealtime + millis;
        }
        if (this.KV < 0) {
            this.KV = 0;
        }
        return this;
    }

    public LocationRequest setExpirationTime(long millis) {
        this.KV = millis;
        if (this.KV < 0) {
            this.KV = 0;
        }
        return this;
    }

    public LocationRequest setFastestInterval(long millis) {
        r(millis);
        this.Le = true;
        this.Ld = millis;
        return this;
    }

    public LocationRequest setInterval(long millis) {
        r(millis);
        this.Lc = millis;
        if (!this.Le) {
            this.Ld = (long) (((double) this.Lc) / 6.0d);
        }
        return this;
    }

    public LocationRequest setNumUpdates(int numUpdates) {
        if (numUpdates <= 0) {
            throw new IllegalArgumentException("invalid numUpdates: " + numUpdates);
        }
        this.Lf = numUpdates;
        return this;
    }

    public LocationRequest setPriority(int priority) {
        bi(priority);
        this.mPriority = priority;
        return this;
    }

    public LocationRequest setSmallestDisplacement(float smallestDisplacementMeters) {
        a(smallestDisplacementMeters);
        this.Lg = smallestDisplacementMeters;
        return this;
    }

    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("Request[").append(bj(this.mPriority));
        if (this.mPriority != PRIORITY_NO_POWER) {
            stringBuilder.append(" requested=");
            stringBuilder.append(this.Lc + "ms");
        }
        stringBuilder.append(" fastest=");
        stringBuilder.append(this.Ld + "ms");
        if (this.KV != Long.MAX_VALUE) {
            long elapsedRealtime = this.KV - SystemClock.elapsedRealtime();
            stringBuilder.append(" expireIn=");
            stringBuilder.append(elapsedRealtime + "ms");
        }
        if (this.Lf != Integer.MAX_VALUE) {
            stringBuilder.append(" num=").append(this.Lf);
        }
        stringBuilder.append(']');
        return stringBuilder.toString();
    }

    public void writeToParcel(Parcel parcel, int flags) {
        LocationRequestCreator.a(this, parcel, flags);
    }
}
