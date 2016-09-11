package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public final class jt implements SafeParcelable {
    public static final Creator<jt> CREATOR;
    String ZN;
    String description;
    private final int wj;

    static {
        CREATOR = new ju();
    }

    jt() {
        this.wj = 1;
    }

    jt(int i, String str, String str2) {
        this.wj = i;
        this.ZN = str;
        this.description = str2;
    }

    public int describeContents() {
        return 0;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel dest, int flags) {
        ju.a(this, dest, flags);
    }
}
