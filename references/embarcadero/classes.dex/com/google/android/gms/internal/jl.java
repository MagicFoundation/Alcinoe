package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public final class jl implements SafeParcelable {
    public static final Creator<jl> CREATOR;
    jr YM;
    jm ZD;
    String label;
    String type;
    private final int wj;

    static {
        CREATOR = new jo();
    }

    jl() {
        this.wj = 1;
    }

    jl(int i, String str, jm jmVar, String str2, jr jrVar) {
        this.wj = i;
        this.label = str;
        this.ZD = jmVar;
        this.type = str2;
        this.YM = jrVar;
    }

    public int describeContents() {
        return 0;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel dest, int flags) {
        jo.a(this, dest, flags);
    }
}
