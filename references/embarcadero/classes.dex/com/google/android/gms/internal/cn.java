package com.google.android.gms.internal;

import java.util.Collections;
import java.util.List;

public final class cn {
    public final int errorCode;
    public final ax mM;
    public final bg mN;
    public final String mO;
    public final ba mP;
    public final List<String> mt;
    public final List<String> mu;
    public final long mx;
    public final dd nu;
    public final z oc;
    public final String of;
    public final long oj;
    public final boolean ok;
    public final long ol;
    public final List<String> om;
    public final int orientation;
    public final ay pf;
    public final ab pg;
    public final long ph;
    public final long pi;

    public cn(z zVar, dd ddVar, List<String> list, int i, List<String> list2, List<String> list3, int i2, long j, String str, boolean z, ax axVar, bg bgVar, String str2, ay ayVar, ba baVar, long j2, ab abVar, long j3, long j4, long j5) {
        this.oc = zVar;
        this.nu = ddVar;
        this.mt = list != null ? Collections.unmodifiableList(list) : null;
        this.errorCode = i;
        this.mu = list2 != null ? Collections.unmodifiableList(list2) : null;
        this.om = list3 != null ? Collections.unmodifiableList(list3) : null;
        this.orientation = i2;
        this.mx = j;
        this.of = str;
        this.ok = z;
        this.mM = axVar;
        this.mN = bgVar;
        this.mO = str2;
        this.pf = ayVar;
        this.mP = baVar;
        this.ol = j2;
        this.pg = abVar;
        this.oj = j3;
        this.ph = j4;
        this.pi = j5;
    }
}
