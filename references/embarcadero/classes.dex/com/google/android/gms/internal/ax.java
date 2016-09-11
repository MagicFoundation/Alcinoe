package com.google.android.gms.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public final class ax {
    public final String adJson;
    public final String mm;
    public final List<String> mn;
    public final String mo;
    public final List<String> mp;
    public final String mq;

    public ax(JSONObject jSONObject) throws JSONException {
        String str = null;
        this.mm = jSONObject.getString("id");
        JSONArray jSONArray = jSONObject.getJSONArray("adapters");
        List arrayList = new ArrayList(jSONArray.length());
        for (int i = 0; i < jSONArray.length(); i++) {
            arrayList.add(jSONArray.getString(i));
        }
        this.mn = Collections.unmodifiableList(arrayList);
        this.mo = jSONObject.optString("allocation_id", null);
        this.mp = bd.a(jSONObject, "imp_urls");
        JSONObject optJSONObject = jSONObject.optJSONObject("ad");
        this.adJson = optJSONObject != null ? optJSONObject.toString() : null;
        optJSONObject = jSONObject.optJSONObject("data");
        if (optJSONObject != null) {
            str = optJSONObject.toString();
        }
        this.mq = str;
    }
}
