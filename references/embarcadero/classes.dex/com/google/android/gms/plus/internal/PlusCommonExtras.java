package com.google.android.gms.plus.internal;

import android.os.Bundle;
import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.internal.ep;

public class PlusCommonExtras implements SafeParcelable {
    public static final f CREATOR;
    public static String TAG;
    private String Rj;
    private String Rk;
    private final int wj;

    static {
        TAG = "PlusCommonExtras";
        CREATOR = new f();
    }

    public PlusCommonExtras() {
        this.wj = 1;
        this.Rj = "";
        this.Rk = "";
    }

    PlusCommonExtras(int versionCode, String gpsrc, String clientCallingPackage) {
        this.wj = versionCode;
        this.Rj = gpsrc;
        this.Rk = clientCallingPackage;
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object obj) {
        if (!(obj instanceof PlusCommonExtras)) {
            return false;
        }
        PlusCommonExtras plusCommonExtras = (PlusCommonExtras) obj;
        return this.wj == plusCommonExtras.wj && ep.equal(this.Rj, plusCommonExtras.Rj) && ep.equal(this.Rk, plusCommonExtras.Rk);
    }

    public int getVersionCode() {
        return this.wj;
    }

    public int hashCode() {
        return ep.hashCode(Integer.valueOf(this.wj), this.Rj, this.Rk);
    }

    public String ho() {
        return this.Rj;
    }

    public String hp() {
        return this.Rk;
    }

    public void m(Bundle bundle) {
    }

    public String toString() {
        return ep.e(this).a("versionCode", Integer.valueOf(this.wj)).a("Gpsrc", this.Rj).a("ClientCallingPackage", this.Rk).toString();
    }

    public void writeToParcel(Parcel out, int flags) {
        f.a(this, out, flags);
    }
}
