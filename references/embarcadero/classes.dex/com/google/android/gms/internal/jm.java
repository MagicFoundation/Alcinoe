package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public final class jm implements SafeParcelable {
    public static final Creator<jm> CREATOR;
    int ZE;
    String ZF;
    double ZG;
    String ZH;
    long ZI;
    int ZJ;
    private final int wj;

    static {
        CREATOR = new jn();
    }

    jm() {
        this.wj = 1;
        this.ZJ = -1;
        this.ZE = -1;
        this.ZG = -1.0d;
    }

    jm(int i, int i2, String str, double d, String str2, long j, int i3) {
        this.wj = i;
        this.ZE = i2;
        this.ZF = str;
        this.ZG = d;
        this.ZH = str2;
        this.ZI = j;
        this.ZJ = i3;
    }

    public int describeContents() {
        return 0;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel dest, int flags) {
        jn.a(this, dest, flags);
    }
}
