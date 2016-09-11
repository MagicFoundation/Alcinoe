package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public final class jp implements SafeParcelable {
    public static final Creator<jp> CREATOR;
    String ZK;
    String oi;
    private final int wj;

    static {
        CREATOR = new jq();
    }

    jp() {
        this.wj = 1;
    }

    jp(int i, String str, String str2) {
        this.wj = i;
        this.ZK = str;
        this.oi = str2;
    }

    public int describeContents() {
        return 0;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel dest, int flags) {
        jq.a(this, dest, flags);
    }
}
