package com.google.android.gms.ads;

import android.content.Context;
import android.location.Location;
import com.google.android.gms.ads.mediation.NetworkExtras;
import com.google.android.gms.internal.aj;
import com.google.android.gms.internal.aj.a;
import java.util.Date;
import java.util.Set;

public final class AdRequest {
    public static final String DEVICE_ID_EMULATOR;
    public static final int ERROR_CODE_INTERNAL_ERROR = 0;
    public static final int ERROR_CODE_INVALID_REQUEST = 1;
    public static final int ERROR_CODE_NETWORK_ERROR = 2;
    public static final int ERROR_CODE_NO_FILL = 3;
    public static final int GENDER_FEMALE = 2;
    public static final int GENDER_MALE = 1;
    public static final int GENDER_UNKNOWN = 0;
    private final aj kA;

    public static final class Builder {
        private final a kB;

        public Builder() {
            this.kB = new a();
        }

        public Builder addKeyword(String keyword) {
            this.kB.g(keyword);
            return this;
        }

        public Builder addNetworkExtras(NetworkExtras networkExtras) {
            this.kB.a(networkExtras);
            return this;
        }

        public Builder addTestDevice(String deviceId) {
            this.kB.h(deviceId);
            return this;
        }

        public AdRequest build() {
            return new AdRequest();
        }

        public Builder setBirthday(Date birthday) {
            this.kB.a(birthday);
            return this;
        }

        public Builder setGender(int gender) {
            this.kB.d(gender);
            return this;
        }

        public Builder setLocation(Location location) {
            this.kB.a(location);
            return this;
        }

        public Builder tagForChildDirectedTreatment(boolean tagForChildDirectedTreatment) {
            this.kB.e(tagForChildDirectedTreatment);
            return this;
        }
    }

    static {
        DEVICE_ID_EMULATOR = aj.DEVICE_ID_EMULATOR;
    }

    private AdRequest(Builder builder) {
        this.kA = new aj(builder.kB);
    }

    aj N() {
        return this.kA;
    }

    public Date getBirthday() {
        return this.kA.getBirthday();
    }

    public int getGender() {
        return this.kA.getGender();
    }

    public Set<String> getKeywords() {
        return this.kA.getKeywords();
    }

    public Location getLocation() {
        return this.kA.getLocation();
    }

    public <T extends NetworkExtras> T getNetworkExtras(Class<T> networkExtrasClass) {
        return this.kA.getNetworkExtras(networkExtrasClass);
    }

    public boolean isTestDevice(Context context) {
        return this.kA.isTestDevice(context);
    }
}
