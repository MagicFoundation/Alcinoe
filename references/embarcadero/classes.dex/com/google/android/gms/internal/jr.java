package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public final class jr implements SafeParcelable {
    public static final Creator<jr> CREATOR;
    long ZL;
    long ZM;
    private final int wj;

    static {
        CREATOR = new js();
    }

    jr() {
        this.wj = 1;
    }

    jr(int i, long j, long j2) {
        this.wj = i;
        this.ZL = j;
        this.ZM = j2;
    }

    public int describeContents() {
        return 0;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel dest, int flags) {
        js.a(this, dest, flags);
    }
}
