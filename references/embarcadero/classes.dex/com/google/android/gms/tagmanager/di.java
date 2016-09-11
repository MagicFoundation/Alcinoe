package com.google.android.gms.tagmanager;

import android.support.v4.media.TransportMediator;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.internal.d.a;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

class di {
    private static final Object XI;
    private static Long XJ;
    private static Double XK;
    private static dh XL;
    private static String XM;
    private static Boolean XN;
    private static List<Object> XO;
    private static Map<Object, Object> XP;
    private static a XQ;

    static {
        XI = null;
        XJ = new Long(0);
        XK = new Double(0.0d);
        XL = dh.v(0);
        XM = new String("");
        XN = new Boolean(false);
        XO = new ArrayList(0);
        XP = new HashMap();
        XQ = r(XM);
    }

    public static a bI(String str) {
        a aVar = new a();
        aVar.type = 5;
        aVar.gd = str;
        return aVar;
    }

    private static dh bJ(String str) {
        try {
            return dh.bH(str);
        } catch (NumberFormatException e) {
            bh.t("Failed to convert '" + str + "' to a number.");
            return XL;
        }
    }

    private static Long bK(String str) {
        dh bJ = bJ(str);
        return bJ == XL ? XJ : Long.valueOf(bJ.longValue());
    }

    private static Double bL(String str) {
        dh bJ = bJ(str);
        return bJ == XL ? XK : Double.valueOf(bJ.doubleValue());
    }

    private static Boolean bM(String str) {
        return "true".equalsIgnoreCase(str) ? Boolean.TRUE : "false".equalsIgnoreCase(str) ? Boolean.FALSE : XN;
    }

    private static double getDouble(Object o) {
        if (o instanceof Number) {
            return ((Number) o).doubleValue();
        }
        bh.t("getDouble received non-Number");
        return 0.0d;
    }

    public static String j(a aVar) {
        return m(o(aVar));
    }

    public static dh k(a aVar) {
        return n(o(aVar));
    }

    public static Object ko() {
        return XI;
    }

    public static Long kp() {
        return XJ;
    }

    public static Double kq() {
        return XK;
    }

    public static Boolean kr() {
        return XN;
    }

    public static dh ks() {
        return XL;
    }

    public static String kt() {
        return XM;
    }

    public static a ku() {
        return XQ;
    }

    public static Long l(a aVar) {
        return o(o(aVar));
    }

    public static Double m(a aVar) {
        return p(o(aVar));
    }

    public static String m(Object obj) {
        return obj == null ? XM : obj.toString();
    }

    public static dh n(Object obj) {
        return obj instanceof dh ? (dh) obj : t(obj) ? dh.v(u(obj)) : s(obj) ? dh.a(Double.valueOf(getDouble(obj))) : bJ(m(obj));
    }

    public static Boolean n(a aVar) {
        return q(o(aVar));
    }

    public static Long o(Object obj) {
        return t(obj) ? Long.valueOf(u(obj)) : bK(m(obj));
    }

    public static Object o(a aVar) {
        int i = 0;
        if (aVar == null) {
            return XI;
        }
        a[] aVarArr;
        int length;
        switch (aVar.type) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return aVar.fY;
            case DetectedActivity.ON_FOOT /*2*/:
                ArrayList arrayList = new ArrayList(aVar.fZ.length);
                aVarArr = aVar.fZ;
                length = aVarArr.length;
                while (i < length) {
                    Object o = o(aVarArr[i]);
                    if (o == XI) {
                        return XI;
                    }
                    arrayList.add(o);
                    i++;
                }
                return arrayList;
            case DetectedActivity.STILL /*3*/:
                if (aVar.ga.length != aVar.gb.length) {
                    bh.t("Converting an invalid value to object: " + aVar.toString());
                    return XI;
                }
                Map hashMap = new HashMap(aVar.gb.length);
                while (i < aVar.ga.length) {
                    Object o2 = o(aVar.ga[i]);
                    Object o3 = o(aVar.gb[i]);
                    if (o2 == XI || o3 == XI) {
                        return XI;
                    }
                    hashMap.put(o2, o3);
                    i++;
                }
                return hashMap;
            case DetectedActivity.UNKNOWN /*4*/:
                bh.t("Trying to convert a macro reference to object");
                return XI;
            case DetectedActivity.TILTING /*5*/:
                bh.t("Trying to convert a function id to object");
                return XI;
            case Participant.STATUS_UNRESPONSIVE /*6*/:
                return Long.valueOf(aVar.ge);
            case Error.AVS_DECLINE /*7*/:
                StringBuffer stringBuffer = new StringBuffer();
                aVarArr = aVar.gg;
                length = aVarArr.length;
                while (i < length) {
                    String j = j(aVarArr[i]);
                    if (j == XM) {
                        return XI;
                    }
                    stringBuffer.append(j);
                    i++;
                }
                return stringBuffer.toString();
            case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                return Boolean.valueOf(aVar.gf);
            default:
                bh.t("Failed to convert a value of type: " + aVar.type);
                return XI;
        }
    }

    public static Double p(Object obj) {
        return s(obj) ? Double.valueOf(getDouble(obj)) : bL(m(obj));
    }

    public static Boolean q(Object obj) {
        return obj instanceof Boolean ? (Boolean) obj : bM(m(obj));
    }

    public static a r(Object obj) {
        boolean z = false;
        a aVar = new a();
        if (obj instanceof a) {
            return (a) obj;
        }
        if (obj instanceof String) {
            aVar.type = 1;
            aVar.fY = (String) obj;
        } else if (obj instanceof List) {
            aVar.type = 2;
            List<Object> list = (List) obj;
            r5 = new ArrayList(list.size());
            r1 = false;
            for (Object r : list) {
                a r2 = r(r);
                if (r2 == XQ) {
                    return XQ;
                }
                r0 = r1 || r2.gi;
                r5.add(r2);
                r1 = r0;
            }
            aVar.fZ = (a[]) r5.toArray(new a[0]);
            z = r1;
        } else if (obj instanceof Map) {
            aVar.type = 3;
            Set<Entry> entrySet = ((Map) obj).entrySet();
            r5 = new ArrayList(entrySet.size());
            List arrayList = new ArrayList(entrySet.size());
            r1 = false;
            for (Entry entry : entrySet) {
                a r3 = r(entry.getKey());
                a r4 = r(entry.getValue());
                if (r3 == XQ || r4 == XQ) {
                    return XQ;
                }
                r0 = r1 || r3.gi || r4.gi;
                r5.add(r3);
                arrayList.add(r4);
                r1 = r0;
            }
            aVar.ga = (a[]) r5.toArray(new a[0]);
            aVar.gb = (a[]) arrayList.toArray(new a[0]);
            z = r1;
        } else if (s(obj)) {
            aVar.type = 1;
            aVar.fY = obj.toString();
        } else if (t(obj)) {
            aVar.type = 6;
            aVar.ge = u(obj);
        } else if (obj instanceof Boolean) {
            aVar.type = 8;
            aVar.gf = ((Boolean) obj).booleanValue();
        } else {
            bh.t("Converting to Value from unknown object type: " + (obj == null ? "null" : obj.getClass().toString()));
            return XQ;
        }
        aVar.gi = z;
        return aVar;
    }

    private static boolean s(Object obj) {
        return (obj instanceof Double) || (obj instanceof Float) || ((obj instanceof dh) && ((dh) obj).kj());
    }

    private static boolean t(Object obj) {
        return (obj instanceof Byte) || (obj instanceof Short) || (obj instanceof Integer) || (obj instanceof Long) || ((obj instanceof dh) && ((dh) obj).kk());
    }

    private static long u(Object obj) {
        if (obj instanceof Number) {
            return ((Number) obj).longValue();
        }
        bh.t("getInt64 received non-Number");
        return 0;
    }
}
