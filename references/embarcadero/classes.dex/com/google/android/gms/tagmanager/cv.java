package com.google.android.gms.tagmanager;

import android.os.Build.VERSION;
import com.google.android.gms.internal.a;
import com.google.android.gms.internal.d;
import java.util.Map;

class cv extends aj {
    private static final String ID;

    static {
        ID = a.SDK_VERSION.toString();
    }

    public cv() {
        super(ID, new String[0]);
    }

    public boolean iy() {
        return true;
    }

    public d.a u(Map<String, d.a> map) {
        return di.r(Integer.valueOf(VERSION.SDK_INT));
    }
}
