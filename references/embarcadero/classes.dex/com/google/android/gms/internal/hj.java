package com.google.android.gms.internal;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.location.Geofence;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.Locale;

public class hj implements SafeParcelable, Geofence {
    public static final hk CREATOR;
    private final String Hh;
    private final int KU;
    private final short KW;
    private final double KX;
    private final double KY;
    private final float KZ;
    private final int La;
    private final int Lb;
    private final long Lz;
    private final int wj;

    static {
        CREATOR = new hk();
    }

    public hj(int i, String str, int i2, short s, double d, double d2, float f, long j, int i3, int i4) {
        aH(str);
        b(f);
        a(d, d2);
        int bn = bn(i2);
        this.wj = i;
        this.KW = s;
        this.Hh = str;
        this.KX = d;
        this.KY = d2;
        this.KZ = f;
        this.Lz = j;
        this.KU = bn;
        this.La = i3;
        this.Lb = i4;
    }

    public hj(String str, int i, short s, double d, double d2, float f, long j, int i2, int i3) {
        this(1, str, i, s, d, d2, f, j, i2, i3);
    }

    private static void a(double d, double d2) {
        if (d > 90.0d || d < -90.0d) {
            throw new IllegalArgumentException("invalid latitude: " + d);
        } else if (d2 > 180.0d || d2 < -180.0d) {
            throw new IllegalArgumentException("invalid longitude: " + d2);
        }
    }

    private static void aH(String str) {
        if (str == null || str.length() > 100) {
            throw new IllegalArgumentException("requestId is null or too long: " + str);
        }
    }

    private static void b(float f) {
        if (f <= 0.0f) {
            throw new IllegalArgumentException("invalid radius: " + f);
        }
    }

    private static int bn(int i) {
        int i2 = i & 7;
        if (i2 != 0) {
            return i2;
        }
        throw new IllegalArgumentException("No supported transition specified: " + i);
    }

    private static String bo(int i) {
        switch (i) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return "CIRCLE";
            default:
                return null;
        }
    }

    public static hj h(byte[] bArr) {
        Parcel obtain = Parcel.obtain();
        obtain.unmarshall(bArr, 0, bArr.length);
        obtain.setDataPosition(0);
        hj av = CREATOR.av(obtain);
        obtain.recycle();
        return av;
    }

    public int describeContents() {
        hk hkVar = CREATOR;
        return 0;
    }

    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof hj)) {
            return false;
        }
        hj hjVar = (hj) obj;
        if (this.KZ != hjVar.KZ) {
            return false;
        }
        if (this.KX != hjVar.KX) {
            return false;
        }
        if (this.KY != hjVar.KY) {
            return false;
        }
        return this.KW == hjVar.KW;
    }

    public long getExpirationTime() {
        return this.Lz;
    }

    public double getLatitude() {
        return this.KX;
    }

    public double getLongitude() {
        return this.KY;
    }

    public int getNotificationResponsiveness() {
        return this.La;
    }

    public String getRequestId() {
        return this.Hh;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public short gn() {
        return this.KW;
    }

    public float go() {
        return this.KZ;
    }

    public int gp() {
        return this.KU;
    }

    public int gq() {
        return this.Lb;
    }

    public int hashCode() {
        long doubleToLongBits = Double.doubleToLongBits(this.KX);
        int i = ((int) (doubleToLongBits ^ (doubleToLongBits >>> 32))) + 31;
        long doubleToLongBits2 = Double.doubleToLongBits(this.KY);
        return (((((((i * 31) + ((int) (doubleToLongBits2 ^ (doubleToLongBits2 >>> 32)))) * 31) + Float.floatToIntBits(this.KZ)) * 31) + this.KW) * 31) + this.KU;
    }

    public String toString() {
        return String.format(Locale.US, "Geofence[%s id:%s transitions:%d %.6f, %.6f %.0fm, resp=%ds, dwell=%dms, @%d]", new Object[]{bo(this.KW), this.Hh, Integer.valueOf(this.KU), Double.valueOf(this.KX), Double.valueOf(this.KY), Float.valueOf(this.KZ), Integer.valueOf(this.La / GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE), Integer.valueOf(this.Lb), Long.valueOf(this.Lz)});
    }

    public void writeToParcel(Parcel parcel, int flags) {
        hk hkVar = CREATOR;
        hk.a(this, parcel, flags);
    }
}
