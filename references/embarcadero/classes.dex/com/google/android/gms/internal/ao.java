package com.google.android.gms.internal;

import java.util.Map;

public final class ao implements ar {
    private final ap lV;

    public ao(ap apVar) {
        this.lV = apVar;
    }

    public void a(dd ddVar, Map<String, String> map) {
        String str = (String) map.get("name");
        if (str == null) {
            da.w("App event with no name parameter.");
        } else {
            this.lV.onAppEvent(str, (String) map.get("info"));
        }
    }
}
