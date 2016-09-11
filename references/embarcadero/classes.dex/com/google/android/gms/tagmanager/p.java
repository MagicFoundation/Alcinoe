package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.a;
import com.google.android.gms.internal.d;
import java.util.Map;

class p extends aj {
    private static final String ID;
    private final String Un;

    static {
        ID = a.CONTAINER_VERSION.toString();
    }

    public p(String str) {
        super(ID, new String[0]);
        this.Un = str;
    }

    public boolean iy() {
        return true;
    }

    public d.a u(Map<String, d.a> map) {
        return this.Un == null ? di.ku() : di.r(this.Un);
    }
}
