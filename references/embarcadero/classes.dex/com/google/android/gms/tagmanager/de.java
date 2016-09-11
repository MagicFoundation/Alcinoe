package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.a;
import com.google.android.gms.internal.d;
import java.util.Map;

class de extends aj {
    private static final String ID;

    static {
        ID = a.TIME.toString();
    }

    public de() {
        super(ID, new String[0]);
    }

    public boolean iy() {
        return false;
    }

    public d.a u(Map<String, d.a> map) {
        return di.r(Long.valueOf(System.currentTimeMillis()));
    }
}
