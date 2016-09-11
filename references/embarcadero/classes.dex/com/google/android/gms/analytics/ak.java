package com.google.android.gms.analytics;

import android.text.TextUtils;
import com.google.analytics.tracking.android.ModelFields;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

class ak {
    private static final char[] uR;

    static {
        uR = new char[]{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
    }

    public static Map<String, String> I(String str) {
        Map<String, String> hashMap = new HashMap();
        for (String split : str.split("&")) {
            String[] split2 = split.split("=");
            if (split2.length > 1) {
                hashMap.put(split2[0], split2[1]);
            } else if (split2.length == 1 && split2[0].length() != 0) {
                hashMap.put(split2[0], null);
            }
        }
        return hashMap;
    }

    public static String J(String str) {
        if (TextUtils.isEmpty(str)) {
            return null;
        }
        if (str.contains("?")) {
            String[] split = str.split("[\\?]");
            if (split.length > 1) {
                str = split[1];
            }
        }
        if (str.contains("%3D")) {
            try {
                str = URLDecoder.decode(str, "UTF-8");
            } catch (UnsupportedEncodingException e) {
                return null;
            }
        } else if (!str.contains("=")) {
            return null;
        }
        Map I = I(str);
        String[] strArr = new String[]{ModelFields.DCLID, "utm_source", ModelFields.GCLID, "utm_campaign", "utm_medium", "utm_term", "utm_content", "utm_id", ModelFields.GMOB_T};
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < strArr.length; i++) {
            if (!TextUtils.isEmpty((CharSequence) I.get(strArr[i]))) {
                if (stringBuilder.length() > 0) {
                    stringBuilder.append("&");
                }
                stringBuilder.append(strArr[i]).append("=").append((String) I.get(strArr[i]));
            }
        }
        return stringBuilder.toString();
    }

    public static double a(String str, double d) {
        if (str != null) {
            try {
                d = Double.parseDouble(str);
            } catch (NumberFormatException e) {
            }
        }
        return d;
    }

    static String a(Locale locale) {
        if (locale == null || TextUtils.isEmpty(locale.getLanguage())) {
            return null;
        }
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(locale.getLanguage().toLowerCase());
        if (!TextUtils.isEmpty(locale.getCountry())) {
            stringBuilder.append("-").append(locale.getCountry().toLowerCase());
        }
        return stringBuilder.toString();
    }

    public static void a(Map<String, String> map, String str, String str2) {
        if (!map.containsKey(str)) {
            map.put(str, str2);
        }
    }

    public static boolean d(String str, boolean z) {
        return str != null ? (str.equalsIgnoreCase("true") || str.equalsIgnoreCase("yes") || str.equalsIgnoreCase("1")) ? true : (str.equalsIgnoreCase("false") || str.equalsIgnoreCase("no") || str.equalsIgnoreCase("0")) ? false : z : z;
    }

    static String s(boolean z) {
        return z ? "1" : "0";
    }
}
