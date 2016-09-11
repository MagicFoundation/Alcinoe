package com.google.android.gms.internal;

import java.util.HashMap;

public class fq {
    public static void a(StringBuilder stringBuilder, HashMap<String, String> hashMap) {
        stringBuilder.append("{");
        Object obj = 1;
        for (String str : hashMap.keySet()) {
            Object obj2;
            if (obj == null) {
                stringBuilder.append(",");
                obj2 = obj;
            } else {
                obj2 = null;
            }
            String str2 = (String) hashMap.get(str);
            stringBuilder.append("\"").append(str).append("\":");
            if (str2 == null) {
                stringBuilder.append("null");
            } else {
                stringBuilder.append("\"").append(str2).append("\"");
            }
            obj = obj2;
        }
        stringBuilder.append("}");
    }
}
