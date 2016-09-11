package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.a;
import com.google.android.gms.internal.d;
import java.util.Map;

class an extends aj {
    private static final String ID;

    static {
        ID = a.GTM_VERSION.toString();
    }

    public an() {
        super(ID, new String[0]);
    }

    public boolean iy() {
        return true;
    }

    public d.a u(Map<String, d.a> map) {
        return di.r("3.02b1");
    }
}
