package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.d.a;
import java.util.Map;

abstract class dd extends cd {
    public dd(String str) {
        super(str);
    }

    protected boolean a(a aVar, a aVar2, Map<String, a> map) {
        String j = di.j(aVar);
        String j2 = di.j(aVar2);
        return (j == di.kt() || j2 == di.kt()) ? false : a(j, j2, (Map) map);
    }

    protected abstract boolean a(String str, String str2, Map<String, a> map);
}
