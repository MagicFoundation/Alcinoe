package com.google.android.gms.tagmanager;

import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.internal.d.a;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

class dl {
    private static by<a> a(by<a> byVar) {
        try {
            return new by(di.r(bO(di.j((a) byVar.getObject()))), byVar.jr());
        } catch (Throwable e) {
            bh.c("Escape URI: unsupported encoding", e);
            return byVar;
        }
    }

    private static by<a> a(by<a> byVar, int i) {
        if (q((a) byVar.getObject())) {
            switch (i) {
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    return a(byVar);
                default:
                    bh.t("Unsupported Value Escaping: " + i);
                    return byVar;
            }
        }
        bh.t("Escaping can only be applied to strings.");
        return byVar;
    }

    static by<a> a(by<a> byVar, int... iArr) {
        by a;
        for (int a2 : iArr) {
            a = a(a, a2);
        }
        return a;
    }

    static String bO(String str) throws UnsupportedEncodingException {
        return URLEncoder.encode(str, "UTF-8").replaceAll("\\+", "%20");
    }

    private static boolean q(a aVar) {
        return di.o(aVar) instanceof String;
    }
}
