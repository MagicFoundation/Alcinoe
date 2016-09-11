package com.google.android.gms.internal;

import android.content.Context;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public final class bd {
    public static List<String> a(JSONObject jSONObject, String str) throws JSONException {
        JSONArray optJSONArray = jSONObject.optJSONArray(str);
        if (optJSONArray == null) {
            return null;
        }
        List arrayList = new ArrayList(optJSONArray.length());
        for (int i = 0; i < optJSONArray.length(); i++) {
            arrayList.add(optJSONArray.getString(i));
        }
        return Collections.unmodifiableList(arrayList);
    }

    public static void a(Context context, String str, cn cnVar, String str2, boolean z, List<String> list) {
        String str3 = z ? "1" : "0";
        for (String replaceAll : list) {
            String replaceAll2 = replaceAll2.replaceAll("@gw_adlocid@", str2).replaceAll("@gw_adnetrefresh@", str3).replaceAll("@gw_qdata@", cnVar.pf.mw).replaceAll("@gw_sdkver@", str).replaceAll("@gw_sessid@", cp.pu).replaceAll("@gw_seqnum@", cnVar.of);
            if (cnVar.mM != null) {
                replaceAll2 = replaceAll2.replaceAll("@gw_adnetid@", cnVar.mM.mm).replaceAll("@gw_allocid@", cnVar.mM.mo);
            }
            new cy(context, str, replaceAll2).start();
        }
    }
}
