package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public final class jh implements SafeParcelable {
    public static final Creator<jh> CREATOR;
    String label;
    String value;
    private final int wj;

    static {
        CREATOR = new ji();
    }

    jh() {
        this.wj = 1;
    }

    jh(int i, String str, String str2) {
        this.wj = i;
        this.label = str;
        this.value = str2;
    }

    public int describeContents() {
        return 0;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel dest, int flags) {
        ji.a(this, dest, flags);
    }
}
