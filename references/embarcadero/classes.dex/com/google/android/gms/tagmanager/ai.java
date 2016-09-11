package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.c.c;
import com.google.android.gms.internal.c.d;
import com.google.android.gms.internal.c.i;
import com.google.android.gms.internal.d.a;
import java.util.Map;

class ai {
    private static void a(DataLayer dataLayer, d dVar) {
        for (a j : dVar.fd) {
            dataLayer.bg(di.j(j));
        }
    }

    public static void a(DataLayer dataLayer, i iVar) {
        if (iVar.fT == null) {
            bh.w("supplemental missing experimentSupplemental");
            return;
        }
        a(dataLayer, iVar.fT);
        b(dataLayer, iVar.fT);
        c(dataLayer, iVar.fT);
    }

    private static void b(DataLayer dataLayer, d dVar) {
        for (a c : dVar.fc) {
            Map c2 = c(c);
            if (c2 != null) {
                dataLayer.push(c2);
            }
        }
    }

    private static Map<String, Object> c(a aVar) {
        Object o = di.o(aVar);
        if (o instanceof Map) {
            return (Map) o;
        }
        bh.w("value: " + o + " is not a map value, ignored.");
        return null;
    }

    private static void c(DataLayer dataLayer, d dVar) {
        for (c cVar : dVar.fe) {
            if (cVar.eX == null) {
                bh.w("GaExperimentRandom: No key");
            } else {
                Object obj = dataLayer.get(cVar.eX);
                Long valueOf = !(obj instanceof Number) ? null : Long.valueOf(((Number) obj).longValue());
                long j = cVar.eY;
                long j2 = cVar.eZ;
                if (!cVar.fa || valueOf == null || valueOf.longValue() < j || valueOf.longValue() > j2) {
                    if (j <= j2) {
                        obj = Long.valueOf(Math.round((Math.random() * ((double) (j2 - j))) + ((double) j)));
                    } else {
                        bh.w("GaExperimentRandom: random range invalid");
                    }
                }
                dataLayer.bg(cVar.eX);
                Map b = dataLayer.b(cVar.eX, obj);
                if (cVar.fb > 0) {
                    if (b.containsKey("gtm")) {
                        Object obj2 = b.get("gtm");
                        if (obj2 instanceof Map) {
                            ((Map) obj2).put("lifetime", Long.valueOf(cVar.fb));
                        } else {
                            bh.w("GaExperimentRandom: gtm not a map");
                        }
                    } else {
                        b.put("gtm", DataLayer.mapOf("lifetime", Long.valueOf(cVar.fb)));
                    }
                }
                dataLayer.push(b);
            }
        }
    }
}
