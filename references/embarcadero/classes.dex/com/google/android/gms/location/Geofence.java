package com.google.android.gms.location;

import android.os.SystemClock;
import com.google.android.gms.internal.hj;

public interface Geofence {
    public static final int GEOFENCE_TRANSITION_DWELL = 4;
    public static final int GEOFENCE_TRANSITION_ENTER = 1;
    public static final int GEOFENCE_TRANSITION_EXIT = 2;
    public static final long NEVER_EXPIRE = -1;

    public static final class Builder {
        private String Hh;
        private int KU;
        private long KV;
        private short KW;
        private double KX;
        private double KY;
        private float KZ;
        private int La;
        private int Lb;

        public Builder() {
            this.Hh = null;
            this.KU = 0;
            this.KV = Long.MIN_VALUE;
            this.KW = (short) -1;
            this.La = 0;
            this.Lb = -1;
        }

        public Geofence build() {
            if (this.Hh == null) {
                throw new IllegalArgumentException("Request ID not set.");
            } else if (this.KU == 0) {
                throw new IllegalArgumentException("Transitions types not set.");
            } else if ((this.KU & Geofence.GEOFENCE_TRANSITION_DWELL) != 0 && this.Lb < 0) {
                throw new IllegalArgumentException("Non-negative loitering delay needs to be set when transition types include GEOFENCE_TRANSITION_DWELLING.");
            } else if (this.KV == Long.MIN_VALUE) {
                throw new IllegalArgumentException("Expiration not set.");
            } else if (this.KW == (short) -1) {
                throw new IllegalArgumentException("Geofence region not set.");
            } else if (this.La >= 0) {
                return new hj(this.Hh, this.KU, (short) 1, this.KX, this.KY, this.KZ, this.KV, this.La, this.Lb);
            } else {
                throw new IllegalArgumentException("Notification responsiveness should be nonnegative.");
            }
        }

        public Builder setCircularRegion(double latitude, double longitude, float radius) {
            this.KW = (short) 1;
            this.KX = latitude;
            this.KY = longitude;
            this.KZ = radius;
            return this;
        }

        public Builder setExpirationDuration(long durationMillis) {
            if (durationMillis < 0) {
                this.KV = Geofence.NEVER_EXPIRE;
            } else {
                this.KV = SystemClock.elapsedRealtime() + durationMillis;
            }
            return this;
        }

        public Builder setLoiteringDelay(int loiteringDelayMs) {
            this.Lb = loiteringDelayMs;
            return this;
        }

        public Builder setNotificationResponsiveness(int notificationResponsivenessMs) {
            this.La = notificationResponsivenessMs;
            return this;
        }

        public Builder setRequestId(String requestId) {
            this.Hh = requestId;
            return this;
        }

        public Builder setTransitionTypes(int transitionTypes) {
            this.KU = transitionTypes;
            return this;
        }
    }

    String getRequestId();
}
