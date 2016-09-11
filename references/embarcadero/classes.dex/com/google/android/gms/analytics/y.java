package com.google.android.gms.analytics;

import android.text.TextUtils;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

class y {
    static String a(x xVar, long j) {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(xVar.ch());
        if (xVar.cj() > 0) {
            long cj = j - xVar.cj();
            if (cj >= 0) {
                stringBuilder.append("&qt").append("=").append(cj);
            }
        }
        stringBuilder.append("&z").append("=").append(xVar.ci());
        return stringBuilder.toString();
    }

    static String encode(String input) {
        try {
            return URLEncoder.encode(input, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new AssertionError("URL encoding failed for: " + input);
        }
    }

    static Map<String, String> s(Map<String, String> map) {
        Map<String, String> hashMap = new HashMap();
        for (Entry entry : map.entrySet()) {
            if (((String) entry.getKey()).startsWith("&") && entry.getValue() != null) {
                CharSequence substring = ((String) entry.getKey()).substring(1);
                if (!TextUtils.isEmpty(substring)) {
                    hashMap.put(substring, entry.getValue());
                }
            }
        }
        return hashMap;
    }
}
