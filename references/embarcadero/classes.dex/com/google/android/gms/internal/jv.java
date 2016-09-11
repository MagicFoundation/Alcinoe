package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public final class jv implements SafeParcelable {
    public static final Creator<jv> CREATOR;
    String ZK;
    jr ZO;
    jt ZP;
    jt ZQ;
    String oi;
    private final int wj;

    static {
        CREATOR = new jw();
    }

    jv() {
        this.wj = 1;
    }

    jv(int i, String str, String str2, jr jrVar, jt jtVar, jt jtVar2) {
        this.wj = i;
        this.ZK = str;
        this.oi = str2;
        this.ZO = jrVar;
        this.ZP = jtVar;
        this.ZQ = jtVar2;
    }

    public int describeContents() {
        return 0;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel dest, int flags) {
        jw.a(this, dest, flags);
    }
}
