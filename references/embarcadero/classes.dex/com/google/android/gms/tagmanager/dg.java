package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.d.a;
import java.util.Map;

abstract class dg extends aj {
    public dg(String str, String... strArr) {
        super(str, strArr);
    }

    public boolean iy() {
        return false;
    }

    public a u(Map<String, a> map) {
        w(map);
        return di.ku();
    }

    public abstract void w(Map<String, a> map);
}
