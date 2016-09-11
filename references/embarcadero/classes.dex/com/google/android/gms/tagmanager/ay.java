package com.google.android.gms.tagmanager;

import android.content.Context;
import android.content.SharedPreferences;
import android.net.Uri;
import com.google.analytics.tracking.android.ModelFields;
import java.util.HashMap;
import java.util.Map;

class ay {
    private static String Vm;
    static Map<String, String> Vn;

    static {
        Vn = new HashMap();
    }

    ay() {
    }

    static void bq(String str) {
        synchronized (ay.class) {
            Vm = str;
        }
    }

    static void c(Context context, String str) {
        cz.a(context, "gtm_install_referrer", ModelFields.REFERRER, str);
        e(context, str);
    }

    static String d(Context context, String str) {
        if (Vm == null) {
            synchronized (ay.class) {
                if (Vm == null) {
                    SharedPreferences sharedPreferences = context.getSharedPreferences("gtm_install_referrer", 0);
                    if (sharedPreferences != null) {
                        Vm = sharedPreferences.getString(ModelFields.REFERRER, "");
                    } else {
                        Vm = "";
                    }
                }
            }
        }
        return l(Vm, str);
    }

    static String e(Context context, String str, String str2) {
        String str3 = (String) Vn.get(str);
        if (str3 == null) {
            SharedPreferences sharedPreferences = context.getSharedPreferences("gtm_click_referrers", 0);
            str3 = sharedPreferences != null ? sharedPreferences.getString(str, "") : "";
            Vn.put(str, str3);
        }
        return l(str3, str2);
    }

    static void e(Context context, String str) {
        String l = l(str, "conv");
        if (l != null && l.length() > 0) {
            Vn.put(l, str);
            cz.a(context, "gtm_click_referrers", l, str);
        }
    }

    static String l(String str, String str2) {
        return str2 == null ? str.length() > 0 ? str : null : Uri.parse("http://hostname/?" + str).getQueryParameter(str2);
    }
}
