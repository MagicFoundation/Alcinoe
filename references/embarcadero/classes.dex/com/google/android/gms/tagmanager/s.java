package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.b;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

class s extends aj {
    private static final String ID;
    private static final String TF;
    private static final String Up;
    private final a Uq;

    public interface a {
        Object b(String str, Map<String, Object> map);
    }

    static {
        ID = com.google.android.gms.internal.a.FUNCTION_CALL.toString();
        Up = b.FUNCTION_CALL_NAME.toString();
        TF = b.ADDITIONAL_PARAMS.toString();
    }

    public s(a aVar) {
        super(ID, Up);
        this.Uq = aVar;
    }

    public boolean iy() {
        return false;
    }

    public com.google.android.gms.internal.d.a u(Map<String, com.google.android.gms.internal.d.a> map) {
        String j = di.j((com.google.android.gms.internal.d.a) map.get(Up));
        Map hashMap = new HashMap();
        com.google.android.gms.internal.d.a aVar = (com.google.android.gms.internal.d.a) map.get(TF);
        if (aVar != null) {
            Object o = di.o(aVar);
            if (o instanceof Map) {
                for (Entry entry : ((Map) o).entrySet()) {
                    hashMap.put(entry.getKey().toString(), entry.getValue());
                }
            } else {
                bh.w("FunctionCallMacro: expected ADDITIONAL_PARAMS to be a map.");
                return di.ku();
            }
        }
        try {
            return di.r(this.Uq.b(j, hashMap));
        } catch (Exception e) {
            bh.w("Custom macro/tag " + j + " threw exception " + e.getMessage());
            return di.ku();
        }
    }
}
