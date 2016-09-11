package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.a;
import com.google.android.gms.internal.b;
import com.google.android.gms.internal.d;
import java.util.List;
import java.util.Map;

class w extends dg {
    private static final String ID;
    private static final String UN;
    private static final String VALUE;
    private final DataLayer TN;

    static {
        ID = a.DATA_LAYER_WRITE.toString();
        VALUE = b.VALUE.toString();
        UN = b.CLEAR_PERSISTENT_DATA_LAYER_PREFIX.toString();
    }

    public w(DataLayer dataLayer) {
        super(ID, VALUE);
        this.TN = dataLayer;
    }

    private void a(d.a aVar) {
        if (aVar != null && aVar != di.ko()) {
            String j = di.j(aVar);
            if (j != di.kt()) {
                this.TN.bg(j);
            }
        }
    }

    private void b(d.a aVar) {
        if (aVar != null && aVar != di.ko()) {
            Object o = di.o(aVar);
            if (o instanceof List) {
                for (Object o2 : (List) o2) {
                    if (o2 instanceof Map) {
                        this.TN.push((Map) o2);
                    }
                }
            }
        }
    }

    public void w(Map<String, d.a> map) {
        b((d.a) map.get(VALUE));
        a((d.a) map.get(UN));
    }
}
