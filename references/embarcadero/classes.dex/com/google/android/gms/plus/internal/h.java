package com.google.android.gms.plus.internal;

import android.os.Bundle;
import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.internal.ep;
import java.util.Arrays;

public class h implements SafeParcelable {
    public static final j CREATOR;
    private final String[] Rm;
    private final String[] Rn;
    private final String[] Ro;
    private final String Rp;
    private final String Rq;
    private final String Rr;
    private final String Rs;
    private final PlusCommonExtras Rt;
    private final String vi;
    private final int wj;

    static {
        CREATOR = new j();
    }

    h(int i, String str, String[] strArr, String[] strArr2, String[] strArr3, String str2, String str3, String str4, String str5, PlusCommonExtras plusCommonExtras) {
        this.wj = i;
        this.vi = str;
        this.Rm = strArr;
        this.Rn = strArr2;
        this.Ro = strArr3;
        this.Rp = str2;
        this.Rq = str3;
        this.Rr = str4;
        this.Rs = str5;
        this.Rt = plusCommonExtras;
    }

    public h(String str, String[] strArr, String[] strArr2, String[] strArr3, String str2, String str3, String str4, PlusCommonExtras plusCommonExtras) {
        this.wj = 1;
        this.vi = str;
        this.Rm = strArr;
        this.Rn = strArr2;
        this.Ro = strArr3;
        this.Rp = str2;
        this.Rq = str3;
        this.Rr = str4;
        this.Rs = null;
        this.Rt = plusCommonExtras;
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object obj) {
        if (!(obj instanceof h)) {
            return false;
        }
        h hVar = (h) obj;
        return this.wj == hVar.wj && ep.equal(this.vi, hVar.vi) && Arrays.equals(this.Rm, hVar.Rm) && Arrays.equals(this.Rn, hVar.Rn) && Arrays.equals(this.Ro, hVar.Ro) && ep.equal(this.Rp, hVar.Rp) && ep.equal(this.Rq, hVar.Rq) && ep.equal(this.Rr, hVar.Rr) && ep.equal(this.Rs, hVar.Rs) && ep.equal(this.Rt, hVar.Rt);
    }

    public String getAccountName() {
        return this.vi;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public int hashCode() {
        return ep.hashCode(Integer.valueOf(this.wj), this.vi, this.Rm, this.Rn, this.Ro, this.Rp, this.Rq, this.Rr, this.Rs, this.Rt);
    }

    public String[] hq() {
        return this.Rm;
    }

    public String[] hr() {
        return this.Rn;
    }

    public String[] hs() {
        return this.Ro;
    }

    public String ht() {
        return this.Rp;
    }

    public String hu() {
        return this.Rq;
    }

    public String hv() {
        return this.Rr;
    }

    public String hw() {
        return this.Rs;
    }

    public PlusCommonExtras hx() {
        return this.Rt;
    }

    public Bundle hy() {
        Bundle bundle = new Bundle();
        bundle.setClassLoader(PlusCommonExtras.class.getClassLoader());
        this.Rt.m(bundle);
        return bundle;
    }

    public String toString() {
        return ep.e(this).a("versionCode", Integer.valueOf(this.wj)).a("accountName", this.vi).a("requestedScopes", this.Rm).a("visibleActivities", this.Rn).a("requiredFeatures", this.Ro).a("packageNameForAuth", this.Rp).a("callingPackageName", this.Rq).a("applicationName", this.Rr).a("extra", this.Rt.toString()).toString();
    }

    public void writeToParcel(Parcel out, int flags) {
        j.a(this, out, flags);
    }
}
