package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import java.util.ArrayList;

public final class jj implements SafeParcelable {
    public static final Creator<jj> CREATOR;
    String ZA;
    String ZB;
    ArrayList<jh> ZC;
    private final int wj;

    static {
        CREATOR = new jk();
    }

    jj() {
        this.wj = 1;
        this.ZC = fj.eH();
    }

    jj(int i, String str, String str2, ArrayList<jh> arrayList) {
        this.wj = i;
        this.ZA = str;
        this.ZB = str2;
        this.ZC = arrayList;
    }

    public int describeContents() {
        return 0;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel dest, int flags) {
        jk.a(this, dest, flags);
    }
}
