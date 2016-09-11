package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.b;
import com.google.android.gms.internal.d.a;
import java.util.Map;

abstract class cd extends aj {
    private static final String US;
    private static final String VQ;

    static {
        US = b.ARG0.toString();
        VQ = b.ARG1.toString();
    }

    public cd(String str) {
        super(str, US, VQ);
    }

    protected abstract boolean a(a aVar, a aVar2, Map<String, a> map);

    public boolean iy() {
        return true;
    }

    public a u(Map<String, a> map) {
        for (a aVar : map.values()) {
            if (aVar == di.ku()) {
                return di.r(Boolean.valueOf(false));
            }
        }
        a aVar2 = (a) map.get(US);
        a aVar3 = (a) map.get(VQ);
        boolean a = (aVar2 == null || aVar3 == null) ? false : a(aVar2, aVar3, map);
        return di.r(Boolean.valueOf(a));
    }
}
