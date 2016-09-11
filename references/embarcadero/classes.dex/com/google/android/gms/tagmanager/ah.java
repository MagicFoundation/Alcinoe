package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.a;
import com.google.android.gms.internal.d;
import java.util.Map;

class ah extends aj {
    private static final String ID;
    private final ct TO;

    static {
        ID = a.EVENT.toString();
    }

    public ah(ct ctVar) {
        super(ID, new String[0]);
        this.TO = ctVar;
    }

    public boolean iy() {
        return false;
    }

    public d.a u(Map<String, d.a> map) {
        String jY = this.TO.jY();
        return jY == null ? di.ku() : di.r(jY);
    }
}
