package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.plus.PlusShare;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

class az extends aj {
    private static final String ID;
    private static final String US;
    private static final String Vo;
    private static final String Vp;
    private static final String Vq;

    /* renamed from: com.google.android.gms.tagmanager.az.1 */
    static /* synthetic */ class AnonymousClass1 {
        static final /* synthetic */ int[] Vr;

        static {
            Vr = new int[a.values().length];
            try {
                Vr[a.URL.ordinal()] = 1;
            } catch (NoSuchFieldError e) {
            }
            try {
                Vr[a.BACKSLASH.ordinal()] = 2;
            } catch (NoSuchFieldError e2) {
            }
            try {
                Vr[a.NONE.ordinal()] = 3;
            } catch (NoSuchFieldError e3) {
            }
        }
    }

    private enum a {
        NONE,
        URL,
        BACKSLASH
    }

    static {
        ID = com.google.android.gms.internal.a.JOINER.toString();
        US = b.ARG0.toString();
        Vo = b.ITEM_SEPARATOR.toString();
        Vp = b.KEY_VALUE_SEPARATOR.toString();
        Vq = b.ESCAPE.toString();
    }

    public az() {
        super(ID, US);
    }

    private String a(String str, a aVar, Set<Character> set) {
        switch (AnonymousClass1.Vr[aVar.ordinal()]) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                try {
                    return dl.bO(str);
                } catch (Throwable e) {
                    bh.c("Joiner: unsupported encoding", e);
                    return str;
                }
            case DetectedActivity.ON_FOOT /*2*/:
                String replace = str.replace("\\", "\\\\");
                String str2 = replace;
                for (Character ch : set) {
                    CharSequence ch2 = ch.toString();
                    str2 = str2.replace(ch2, "\\" + ch2);
                }
                return str2;
            default:
                return str;
        }
    }

    private void a(StringBuilder stringBuilder, String str, a aVar, Set<Character> set) {
        stringBuilder.append(a(str, aVar, set));
    }

    private void a(Set<Character> set, String str) {
        for (int i = 0; i < str.length(); i++) {
            set.add(Character.valueOf(str.charAt(i)));
        }
    }

    public boolean iy() {
        return true;
    }

    public com.google.android.gms.internal.d.a u(Map<String, com.google.android.gms.internal.d.a> map) {
        com.google.android.gms.internal.d.a aVar = (com.google.android.gms.internal.d.a) map.get(US);
        if (aVar == null) {
            return di.ku();
        }
        a aVar2;
        Set set;
        com.google.android.gms.internal.d.a aVar3 = (com.google.android.gms.internal.d.a) map.get(Vo);
        String j = aVar3 != null ? di.j(aVar3) : "";
        aVar3 = (com.google.android.gms.internal.d.a) map.get(Vp);
        String j2 = aVar3 != null ? di.j(aVar3) : "=";
        a aVar4 = a.NONE;
        aVar3 = (com.google.android.gms.internal.d.a) map.get(Vq);
        if (aVar3 != null) {
            String j3 = di.j(aVar3);
            if (PlusShare.KEY_CALL_TO_ACTION_URL.equals(j3)) {
                aVar2 = a.URL;
                set = null;
            } else if ("backslash".equals(j3)) {
                aVar2 = a.BACKSLASH;
                set = new HashSet();
                a(set, j);
                a(set, j2);
                set.remove(Character.valueOf('\\'));
            } else {
                bh.t("Joiner: unsupported escape type: " + j3);
                return di.ku();
            }
        }
        set = null;
        aVar2 = aVar4;
        StringBuilder stringBuilder = new StringBuilder();
        switch (aVar.type) {
            case DetectedActivity.ON_FOOT /*2*/:
                Object obj = 1;
                com.google.android.gms.internal.d.a[] aVarArr = aVar.fZ;
                int length = aVarArr.length;
                int i = 0;
                while (i < length) {
                    com.google.android.gms.internal.d.a aVar5 = aVarArr[i];
                    if (obj == null) {
                        stringBuilder.append(j);
                    }
                    a(stringBuilder, di.j(aVar5), aVar2, set);
                    i++;
                    obj = null;
                }
                break;
            case DetectedActivity.STILL /*3*/:
                for (int i2 = 0; i2 < aVar.ga.length; i2++) {
                    if (i2 > 0) {
                        stringBuilder.append(j);
                    }
                    String j4 = di.j(aVar.ga[i2]);
                    String j5 = di.j(aVar.gb[i2]);
                    a(stringBuilder, j4, aVar2, set);
                    stringBuilder.append(j2);
                    a(stringBuilder, j5, aVar2, set);
                }
                break;
            default:
                a(stringBuilder, di.j(aVar), aVar2, set);
                break;
        }
        return di.r(stringBuilder.toString());
    }
}
