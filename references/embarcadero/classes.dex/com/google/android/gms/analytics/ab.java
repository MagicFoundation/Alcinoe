package com.google.android.gms.analytics;

import java.util.HashMap;
import java.util.Map;

class ab {
    private final Map<String, Integer> tW;
    private final Map<String, String> tX;
    private final boolean tY;
    private final String tZ;

    ab(String str, boolean z) {
        this.tW = new HashMap();
        this.tX = new HashMap();
        this.tY = z;
        this.tZ = str;
    }

    void c(String str, int i) {
        if (this.tY) {
            Integer num = (Integer) this.tW.get(str);
            if (num == null) {
                num = Integer.valueOf(0);
            }
            this.tW.put(str, Integer.valueOf(num.intValue() + i));
        }
    }

    String cn() {
        if (!this.tY) {
            return "";
        }
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(this.tZ);
        for (String str : this.tW.keySet()) {
            stringBuilder.append("&").append(str).append("=").append(this.tW.get(str));
        }
        for (String str2 : this.tX.keySet()) {
            stringBuilder.append("&").append(str2).append("=").append((String) this.tX.get(str2));
        }
        return stringBuilder.toString();
    }
}
