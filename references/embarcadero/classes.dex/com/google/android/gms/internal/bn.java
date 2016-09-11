package com.google.android.gms.internal;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public final class bn implements SafeParcelable {
    public static final bm CREATOR;
    public final String mY;
    public final String mZ;
    public final String mimeType;
    public final String na;
    public final String nb;
    public final String nc;
    public final String packageName;
    public final int versionCode;

    static {
        CREATOR = new bm();
    }

    public bn(int i, String str, String str2, String str3, String str4, String str5, String str6, String str7) {
        this.versionCode = i;
        this.mY = str;
        this.mZ = str2;
        this.mimeType = str3;
        this.packageName = str4;
        this.na = str5;
        this.nb = str6;
        this.nc = str7;
    }

    public bn(String str, String str2, String str3, String str4, String str5, String str6, String str7) {
        this(1, str, str2, str3, str4, str5, str6, str7);
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel out, int flags) {
        bm.a(this, out, flags);
    }
}
