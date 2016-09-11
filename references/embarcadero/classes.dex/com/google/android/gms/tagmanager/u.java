package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.a;
import com.google.android.gms.internal.b;
import com.google.android.gms.internal.d;
import java.util.Map;

class u extends aj {
    private static final String ID;
    private static final String NAME;
    private static final String UC;
    private final DataLayer TN;

    static {
        ID = a.CUSTOM_VAR.toString();
        NAME = b.NAME.toString();
        UC = b.DEFAULT_VALUE.toString();
    }

    public u(DataLayer dataLayer) {
        super(ID, NAME);
        this.TN = dataLayer;
    }

    public boolean iy() {
        return false;
    }

    public d.a u(Map<String, d.a> map) {
        Object obj = this.TN.get(di.j((d.a) map.get(NAME)));
        if (obj != null) {
            return di.r(obj);
        }
        d.a aVar = (d.a) map.get(UC);
        return aVar != null ? aVar : di.ku();
    }
}
