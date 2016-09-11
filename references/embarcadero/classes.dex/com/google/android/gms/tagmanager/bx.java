package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.d.a;
import java.util.Map;

abstract class bx extends cd {
    public bx(String str) {
        super(str);
    }

    protected boolean a(a aVar, a aVar2, Map<String, a> map) {
        dh k = di.k(aVar);
        dh k2 = di.k(aVar2);
        return (k == di.ks() || k2 == di.ks()) ? false : a(k, k2, (Map) map);
    }

    protected abstract boolean a(dh dhVar, dh dhVar2, Map<String, a> map);
}
