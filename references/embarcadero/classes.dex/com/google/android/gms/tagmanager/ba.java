package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.b;
import com.google.android.gms.internal.d.a;
import com.google.android.gms.tagmanager.cr.c;
import com.google.android.gms.tagmanager.cr.d;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

class ba {
    public static c br(String str) throws JSONException {
        a k = k(new JSONObject(str));
        d jI = c.jI();
        for (int i = 0; i < k.ga.length; i++) {
            jI.a(cr.a.jE().b(b.INSTANCE_NAME.toString(), k.ga[i]).b(b.FUNCTION.toString(), di.bI(m.iB())).b(m.iC(), k.gb[i]).jH());
        }
        return jI.jL();
    }

    private static a k(Object obj) throws JSONException {
        return di.r(l(obj));
    }

    static Object l(Object obj) throws JSONException {
        if (obj instanceof JSONArray) {
            throw new RuntimeException("JSONArrays are not supported");
        } else if (JSONObject.NULL.equals(obj)) {
            throw new RuntimeException("JSON nulls are not supported");
        } else if (!(obj instanceof JSONObject)) {
            return obj;
        } else {
            JSONObject jSONObject = (JSONObject) obj;
            Map hashMap = new HashMap();
            Iterator keys = jSONObject.keys();
            while (keys.hasNext()) {
                String str = (String) keys.next();
                hashMap.put(str, l(jSONObject.get(str)));
            }
            return hashMap;
        }
    }
}
