package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.d.a;
import java.util.Map;

class cc extends aj {
    private static final String ID;
    private static final a VP;

    static {
        ID = com.google.android.gms.internal.a.PLATFORM.toString();
        VP = di.r("Android");
    }

    public cc() {
        super(ID, new String[0]);
    }

    public boolean iy() {
        return true;
    }

    public a u(Map<String, a> map) {
        return VP;
    }
}
