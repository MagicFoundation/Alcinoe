package com.google.android.gms.internal;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import java.util.Collections;
import java.util.List;

public final class cf implements SafeParcelable {
    public static final cg CREATOR;
    public final int errorCode;
    public final List<String> mt;
    public final List<String> mu;
    public final long mx;
    public final String nw;
    public final String oi;
    public final long oj;
    public final boolean ok;
    public final long ol;
    public final List<String> om;
    public final String on;
    public final long oo;
    public final int orientation;
    public final int versionCode;

    static {
        CREATOR = new cg();
    }

    public cf(int i) {
        this(3, null, null, null, i, null, -1, false, -1, null, -1, -1, null, -1);
    }

    cf(int i, String str, String str2, List<String> list, int i2, List<String> list2, long j, boolean z, long j2, List<String> list3, long j3, int i3, String str3, long j4) {
        this.versionCode = i;
        this.nw = str;
        this.oi = str2;
        this.mt = list != null ? Collections.unmodifiableList(list) : null;
        this.errorCode = i2;
        this.mu = list2 != null ? Collections.unmodifiableList(list2) : null;
        this.oj = j;
        this.ok = z;
        this.ol = j2;
        this.om = list3 != null ? Collections.unmodifiableList(list3) : null;
        this.mx = j3;
        this.orientation = i3;
        this.on = str3;
        this.oo = j4;
    }

    public cf(String str, String str2, List<String> list, List<String> list2, long j, boolean z, long j2, List<String> list3, long j3, int i, String str3, long j4) {
        this(3, str, str2, list, -2, list2, j, z, j2, list3, j3, i, str3, j4);
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel out, int flags) {
        cg.a(this, out, flags);
    }
}
