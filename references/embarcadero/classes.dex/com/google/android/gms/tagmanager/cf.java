package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.a;
import com.google.android.gms.internal.b;
import com.google.android.gms.internal.d;
import java.util.Map;

class cf extends aj {
    private static final String ID;
    private static final String Wa;
    private static final String Wb;

    static {
        ID = a.RANDOM.toString();
        Wa = b.MIN.toString();
        Wb = b.MAX.toString();
    }

    public cf() {
        super(ID, new String[0]);
    }

    public boolean iy() {
        return false;
    }

    public d.a u(Map<String, d.a> map) {
        double doubleValue;
        double d;
        d.a aVar = (d.a) map.get(Wa);
        d.a aVar2 = (d.a) map.get(Wb);
        if (!(aVar == null || aVar == di.ku() || aVar2 == null || aVar2 == di.ku())) {
            dh k = di.k(aVar);
            dh k2 = di.k(aVar2);
            if (!(k == di.ks() || k2 == di.ks())) {
                double doubleValue2 = k.doubleValue();
                doubleValue = k2.doubleValue();
                if (doubleValue2 <= doubleValue) {
                    d = doubleValue2;
                    return di.r(Long.valueOf(Math.round(((doubleValue - d) * Math.random()) + d)));
                }
            }
        }
        doubleValue = 2.147483647E9d;
        d = 0.0d;
        return di.r(Long.valueOf(Math.round(((doubleValue - d) * Math.random()) + d)));
    }
}
