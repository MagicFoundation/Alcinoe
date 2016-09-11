package com.google.android.gms.tagmanager;

import android.content.Context;
import android.net.Uri;
import com.google.analytics.tracking.android.ModelFields;
import com.google.android.gms.plus.PlusShare;
import java.util.Map;

class d implements b {
    private final Context kL;

    public d(Context context) {
        this.kL = context;
    }

    public void v(Map<String, Object> map) {
        Object obj;
        Object obj2 = map.get("gtm.url");
        if (obj2 == null) {
            obj = map.get("gtm");
            if (obj != null && (obj instanceof Map)) {
                obj = ((Map) obj).get(PlusShare.KEY_CALL_TO_ACTION_URL);
                if (obj != null && (obj instanceof String)) {
                    String queryParameter = Uri.parse((String) obj).getQueryParameter(ModelFields.REFERRER);
                    if (queryParameter != null) {
                        ay.e(this.kL, queryParameter);
                        return;
                    }
                    return;
                }
            }
        }
        obj = obj2;
        if (obj != null) {
        }
    }
}
