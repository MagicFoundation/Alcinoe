package com.google.android.gms.internal;

import android.content.Intent;
import android.os.Bundle;
import android.os.IBinder;
import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.dynamic.b.a;
import com.google.android.gms.dynamic.c;

public final class bq implements SafeParcelable {
    public static final bp CREATOR;
    public final db kN;
    public final String mZ;
    public final int nA;
    public final bn nr;
    public final u ns;
    public final br nt;
    public final dd nu;
    public final ap nv;
    public final String nw;
    public final boolean nx;
    public final String ny;
    public final bu nz;
    public final int orientation;
    public final int versionCode;

    static {
        CREATOR = new bp();
    }

    bq(int i, bn bnVar, IBinder iBinder, IBinder iBinder2, IBinder iBinder3, IBinder iBinder4, String str, boolean z, String str2, IBinder iBinder5, int i2, int i3, String str3, db dbVar) {
        this.versionCode = i;
        this.nr = bnVar;
        this.ns = (u) c.b(a.G(iBinder));
        this.nt = (br) c.b(a.G(iBinder2));
        this.nu = (dd) c.b(a.G(iBinder3));
        this.nv = (ap) c.b(a.G(iBinder4));
        this.nw = str;
        this.nx = z;
        this.ny = str2;
        this.nz = (bu) c.b(a.G(iBinder5));
        this.orientation = i2;
        this.nA = i3;
        this.mZ = str3;
        this.kN = dbVar;
    }

    public bq(bn bnVar, u uVar, br brVar, bu buVar, db dbVar) {
        this.versionCode = 1;
        this.nr = bnVar;
        this.ns = uVar;
        this.nt = brVar;
        this.nu = null;
        this.nv = null;
        this.nw = null;
        this.nx = false;
        this.ny = null;
        this.nz = buVar;
        this.orientation = -1;
        this.nA = 4;
        this.mZ = null;
        this.kN = dbVar;
    }

    public bq(u uVar, br brVar, ap apVar, bu buVar, dd ddVar, boolean z, int i, String str, db dbVar) {
        this.versionCode = 1;
        this.nr = null;
        this.ns = uVar;
        this.nt = brVar;
        this.nu = ddVar;
        this.nv = apVar;
        this.nw = null;
        this.nx = z;
        this.ny = null;
        this.nz = buVar;
        this.orientation = i;
        this.nA = 3;
        this.mZ = str;
        this.kN = dbVar;
    }

    public bq(u uVar, br brVar, ap apVar, bu buVar, dd ddVar, boolean z, int i, String str, String str2, db dbVar) {
        this.versionCode = 1;
        this.nr = null;
        this.ns = uVar;
        this.nt = brVar;
        this.nu = ddVar;
        this.nv = apVar;
        this.nw = str2;
        this.nx = z;
        this.ny = str;
        this.nz = buVar;
        this.orientation = i;
        this.nA = 3;
        this.mZ = null;
        this.kN = dbVar;
    }

    public bq(u uVar, br brVar, bu buVar, dd ddVar, int i, db dbVar) {
        this.versionCode = 1;
        this.nr = null;
        this.ns = uVar;
        this.nt = brVar;
        this.nu = ddVar;
        this.nv = null;
        this.nw = null;
        this.nx = false;
        this.ny = null;
        this.nz = buVar;
        this.orientation = i;
        this.nA = 1;
        this.mZ = null;
        this.kN = dbVar;
    }

    public bq(u uVar, br brVar, bu buVar, dd ddVar, boolean z, int i, db dbVar) {
        this.versionCode = 1;
        this.nr = null;
        this.ns = uVar;
        this.nt = brVar;
        this.nu = ddVar;
        this.nv = null;
        this.nw = null;
        this.nx = z;
        this.ny = null;
        this.nz = buVar;
        this.orientation = i;
        this.nA = 2;
        this.mZ = null;
        this.kN = dbVar;
    }

    public static bq a(Intent intent) {
        try {
            Bundle bundleExtra = intent.getBundleExtra("com.google.android.gms.ads.inernal.overlay.AdOverlayInfo");
            bundleExtra.setClassLoader(bq.class.getClassLoader());
            return (bq) bundleExtra.getParcelable("com.google.android.gms.ads.inernal.overlay.AdOverlayInfo");
        } catch (Exception e) {
            return null;
        }
    }

    public static void a(Intent intent, bq bqVar) {
        Bundle bundle = new Bundle(1);
        bundle.putParcelable("com.google.android.gms.ads.inernal.overlay.AdOverlayInfo", bqVar);
        intent.putExtra("com.google.android.gms.ads.inernal.overlay.AdOverlayInfo", bundle);
    }

    IBinder at() {
        return c.h(this.ns).asBinder();
    }

    IBinder au() {
        return c.h(this.nt).asBinder();
    }

    IBinder av() {
        return c.h(this.nu).asBinder();
    }

    IBinder aw() {
        return c.h(this.nv).asBinder();
    }

    IBinder ax() {
        return c.h(this.nz).asBinder();
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel out, int flags) {
        bp.a(this, out, flags);
    }
}
