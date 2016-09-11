package com.google.android.gms.internal;

import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import com.google.android.gms.cast.Cast;
import java.util.HashMap;
import java.util.Map;

public final class aq {
    public static final ar lW;
    public static final ar lX;
    public static final ar lY;
    public static final ar lZ;
    public static final ar ma;
    public static final ar mb;
    public static final ar mc;
    public static final ar md;
    public static final ar me;

    static {
        lW = new ar() {
            public void a(dd ddVar, Map<String, String> map) {
                String str = (String) map.get("urls");
                if (str == null) {
                    da.w("URLs missing in canOpenURLs GMSG.");
                    return;
                }
                String[] split = str.split(",");
                Map hashMap = new HashMap();
                PackageManager packageManager = ddVar.getContext().getPackageManager();
                for (String str2 : split) {
                    String[] split2 = str2.split(";", 2);
                    hashMap.put(str2, Boolean.valueOf(packageManager.resolveActivity(new Intent(split2.length > 1 ? split2[1].trim() : "android.intent.action.VIEW", Uri.parse(split2[0].trim())), Cast.MAX_MESSAGE_LENGTH) != null));
                }
                ddVar.a("openableURLs", hashMap);
            }
        };
        lX = new ar() {
            public void a(dd ddVar, Map<String, String> map) {
                String str = (String) map.get("u");
                if (str == null) {
                    da.w("URL missing from click GMSG.");
                    return;
                }
                Uri a;
                Uri parse = Uri.parse(str);
                try {
                    l bc = ddVar.bc();
                    if (bc != null && bc.a(parse)) {
                        a = bc.a(parse, ddVar.getContext());
                        new cy(ddVar.getContext(), ddVar.bd().pU, a.toString()).start();
                    }
                } catch (m e) {
                    da.w("Unable to append parameter to URL: " + str);
                }
                a = parse;
                new cy(ddVar.getContext(), ddVar.bd().pU, a.toString()).start();
            }
        };
        lY = new ar() {
            public void a(dd ddVar, Map<String, String> map) {
                bo ba = ddVar.ba();
                if (ba == null) {
                    da.w("A GMSG tried to close something that wasn't an overlay.");
                } else {
                    ba.close();
                }
            }
        };
        lZ = new ar() {
            public void a(dd ddVar, Map<String, String> map) {
                bo ba = ddVar.ba();
                if (ba == null) {
                    da.w("A GMSG tried to use a custom close button on something that wasn't an overlay.");
                } else {
                    ba.g("1".equals(map.get("custom_close")));
                }
            }
        };
        ma = new ar() {
            public void a(dd ddVar, Map<String, String> map) {
                String str = (String) map.get("u");
                if (str == null) {
                    da.w("URL missing from httpTrack GMSG.");
                } else {
                    new cy(ddVar.getContext(), ddVar.bd().pU, str).start();
                }
            }
        };
        mb = new ar() {
            public void a(dd ddVar, Map<String, String> map) {
                da.u("Received log message: " + ((String) map.get("string")));
            }
        };
        mc = new as();
        md = new ar() {
            public void a(dd ddVar, Map<String, String> map) {
                String str = (String) map.get("ty");
                String str2 = (String) map.get("td");
                try {
                    int parseInt = Integer.parseInt((String) map.get("tx"));
                    int parseInt2 = Integer.parseInt(str);
                    int parseInt3 = Integer.parseInt(str2);
                    l bc = ddVar.bc();
                    if (bc != null) {
                        bc.y().a(parseInt, parseInt2, parseInt3);
                    }
                } catch (NumberFormatException e) {
                    da.w("Could not parse touch parameters from gmsg.");
                }
            }
        };
        me = new at();
    }
}
