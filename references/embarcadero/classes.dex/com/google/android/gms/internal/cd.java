package com.google.android.gms.internal;

import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.os.Bundle;
import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public final class cd implements SafeParcelable {
    public static final ce CREATOR;
    public final String adUnitId;
    public final ApplicationInfo applicationInfo;
    public final db kN;
    public final ab kQ;
    public final Bundle ob;
    public final z oc;
    public final PackageInfo od;
    public final String oe;
    public final String of;
    public final String og;
    public final Bundle oh;
    public final int versionCode;

    public static final class a {
        public final String adUnitId;
        public final ApplicationInfo applicationInfo;
        public final db kN;
        public final ab kQ;
        public final Bundle ob;
        public final z oc;
        public final PackageInfo od;
        public final String of;
        public final String og;
        public final Bundle oh;

        public a(Bundle bundle, z zVar, ab abVar, String str, ApplicationInfo applicationInfo, PackageInfo packageInfo, String str2, String str3, db dbVar, Bundle bundle2) {
            this.ob = bundle;
            this.oc = zVar;
            this.kQ = abVar;
            this.adUnitId = str;
            this.applicationInfo = applicationInfo;
            this.od = packageInfo;
            this.of = str2;
            this.og = str3;
            this.kN = dbVar;
            this.oh = bundle2;
        }
    }

    static {
        CREATOR = new ce();
    }

    cd(int i, Bundle bundle, z zVar, ab abVar, String str, ApplicationInfo applicationInfo, PackageInfo packageInfo, String str2, String str3, String str4, db dbVar, Bundle bundle2) {
        this.versionCode = i;
        this.ob = bundle;
        this.oc = zVar;
        this.kQ = abVar;
        this.adUnitId = str;
        this.applicationInfo = applicationInfo;
        this.od = packageInfo;
        this.oe = str2;
        this.of = str3;
        this.og = str4;
        this.kN = dbVar;
        this.oh = bundle2;
    }

    public cd(Bundle bundle, z zVar, ab abVar, String str, ApplicationInfo applicationInfo, PackageInfo packageInfo, String str2, String str3, String str4, db dbVar, Bundle bundle2) {
        this(2, bundle, zVar, abVar, str, applicationInfo, packageInfo, str2, str3, str4, dbVar, bundle2);
    }

    public cd(a aVar, String str) {
        this(aVar.ob, aVar.oc, aVar.kQ, aVar.adUnitId, aVar.applicationInfo, aVar.od, str, aVar.of, aVar.og, aVar.kN, aVar.oh);
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel out, int flags) {
        ce.a(this, out, flags);
    }
}
