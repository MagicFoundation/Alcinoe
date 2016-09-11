package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.a;
import com.google.android.gms.internal.b;
import com.google.android.gms.internal.d;
import java.util.Map;

class m extends aj {
    private static final String ID;
    private static final String VALUE;

    static {
        ID = a.CONSTANT.toString();
        VALUE = b.VALUE.toString();
    }

    public m() {
        super(ID, VALUE);
    }

    public static String iB() {
        return ID;
    }

    public static String iC() {
        return VALUE;
    }

    public boolean iy() {
        return true;
    }

    public d.a u(Map<String, d.a> map) {
        return (d.a) map.get(VALUE);
    }
}
