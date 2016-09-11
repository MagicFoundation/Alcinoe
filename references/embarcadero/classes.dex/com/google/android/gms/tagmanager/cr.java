package com.google.android.gms.tagmanager;

import android.support.v4.media.TransportMediator;
import android.support.v4.view.accessibility.AccessibilityNodeInfoCompat;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.internal.c.h;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

class cr {

    public static class a {
        private final Map<String, com.google.android.gms.internal.d.a> Ws;
        private final com.google.android.gms.internal.d.a Wt;

        private a(Map<String, com.google.android.gms.internal.d.a> map, com.google.android.gms.internal.d.a aVar) {
            this.Ws = map;
            this.Wt = aVar;
        }

        public static b jE() {
            return new b();
        }

        public void a(String str, com.google.android.gms.internal.d.a aVar) {
            this.Ws.put(str, aVar);
        }

        public Map<String, com.google.android.gms.internal.d.a> jF() {
            return Collections.unmodifiableMap(this.Ws);
        }

        public com.google.android.gms.internal.d.a jG() {
            return this.Wt;
        }

        public String toString() {
            return "Properties: " + jF() + " pushAfterEvaluate: " + this.Wt;
        }
    }

    public static class b {
        private final Map<String, com.google.android.gms.internal.d.a> Ws;
        private com.google.android.gms.internal.d.a Wt;

        private b() {
            this.Ws = new HashMap();
        }

        public b b(String str, com.google.android.gms.internal.d.a aVar) {
            this.Ws.put(str, aVar);
            return this;
        }

        public b i(com.google.android.gms.internal.d.a aVar) {
            this.Wt = aVar;
            return this;
        }

        public a jH() {
            return new a(this.Wt, null);
        }
    }

    public static class c {
        private final String Un;
        private final List<e> Wu;
        private final Map<String, List<a>> Wv;
        private final int Ww;

        private c(List<e> list, Map<String, List<a>> map, String str, int i) {
            this.Wu = Collections.unmodifiableList(list);
            this.Wv = Collections.unmodifiableMap(map);
            this.Un = str;
            this.Ww = i;
        }

        public static d jI() {
            return new d();
        }

        public String getVersion() {
            return this.Un;
        }

        public List<e> jJ() {
            return this.Wu;
        }

        public Map<String, List<a>> jK() {
            return this.Wv;
        }

        public String toString() {
            return "Rules: " + jJ() + "  Macros: " + this.Wv;
        }
    }

    public static class d {
        private String Un;
        private final List<e> Wu;
        private final Map<String, List<a>> Wv;
        private int Ww;

        private d() {
            this.Wu = new ArrayList();
            this.Wv = new HashMap();
            this.Un = "";
            this.Ww = 0;
        }

        public d a(a aVar) {
            String j = di.j((com.google.android.gms.internal.d.a) aVar.jF().get(com.google.android.gms.internal.b.INSTANCE_NAME.toString()));
            List list = (List) this.Wv.get(j);
            if (list == null) {
                list = new ArrayList();
                this.Wv.put(j, list);
            }
            list.add(aVar);
            return this;
        }

        public d a(e eVar) {
            this.Wu.add(eVar);
            return this;
        }

        public d bW(int i) {
            this.Ww = i;
            return this;
        }

        public d bx(String str) {
            this.Un = str;
            return this;
        }

        public c jL() {
            return new c(this.Wv, this.Un, this.Ww, null);
        }
    }

    public static class e {
        private final List<a> WA;
        private final List<a> WB;
        private final List<a> WC;
        private final List<String> WD;
        private final List<String> WE;
        private final List<String> WF;
        private final List<String> WG;
        private final List<a> Wx;
        private final List<a> Wy;
        private final List<a> Wz;

        private e(List<a> list, List<a> list2, List<a> list3, List<a> list4, List<a> list5, List<a> list6, List<String> list7, List<String> list8, List<String> list9, List<String> list10) {
            this.Wx = Collections.unmodifiableList(list);
            this.Wy = Collections.unmodifiableList(list2);
            this.Wz = Collections.unmodifiableList(list3);
            this.WA = Collections.unmodifiableList(list4);
            this.WB = Collections.unmodifiableList(list5);
            this.WC = Collections.unmodifiableList(list6);
            this.WD = Collections.unmodifiableList(list7);
            this.WE = Collections.unmodifiableList(list8);
            this.WF = Collections.unmodifiableList(list9);
            this.WG = Collections.unmodifiableList(list10);
        }

        public static f jM() {
            return new f();
        }

        public List<a> jN() {
            return this.Wx;
        }

        public List<a> jO() {
            return this.Wy;
        }

        public List<a> jP() {
            return this.Wz;
        }

        public List<a> jQ() {
            return this.WA;
        }

        public List<a> jR() {
            return this.WB;
        }

        public List<String> jS() {
            return this.WD;
        }

        public List<String> jT() {
            return this.WE;
        }

        public List<String> jU() {
            return this.WF;
        }

        public List<String> jV() {
            return this.WG;
        }

        public List<a> jW() {
            return this.WC;
        }

        public String toString() {
            return "Positive predicates: " + jN() + "  Negative predicates: " + jO() + "  Add tags: " + jP() + "  Remove tags: " + jQ() + "  Add macros: " + jR() + "  Remove macros: " + jW();
        }
    }

    public static class f {
        private final List<a> WA;
        private final List<a> WB;
        private final List<a> WC;
        private final List<String> WD;
        private final List<String> WE;
        private final List<String> WF;
        private final List<String> WG;
        private final List<a> Wx;
        private final List<a> Wy;
        private final List<a> Wz;

        private f() {
            this.Wx = new ArrayList();
            this.Wy = new ArrayList();
            this.Wz = new ArrayList();
            this.WA = new ArrayList();
            this.WB = new ArrayList();
            this.WC = new ArrayList();
            this.WD = new ArrayList();
            this.WE = new ArrayList();
            this.WF = new ArrayList();
            this.WG = new ArrayList();
        }

        public f b(a aVar) {
            this.Wx.add(aVar);
            return this;
        }

        public f bA(String str) {
            this.WD.add(str);
            return this;
        }

        public f bB(String str) {
            this.WE.add(str);
            return this;
        }

        public f by(String str) {
            this.WF.add(str);
            return this;
        }

        public f bz(String str) {
            this.WG.add(str);
            return this;
        }

        public f c(a aVar) {
            this.Wy.add(aVar);
            return this;
        }

        public f d(a aVar) {
            this.Wz.add(aVar);
            return this;
        }

        public f e(a aVar) {
            this.WA.add(aVar);
            return this;
        }

        public f f(a aVar) {
            this.WB.add(aVar);
            return this;
        }

        public f g(a aVar) {
            this.WC.add(aVar);
            return this;
        }

        public e jX() {
            return new e(this.Wy, this.Wz, this.WA, this.WB, this.WC, this.WD, this.WE, this.WF, this.WG, null);
        }
    }

    public static class g extends Exception {
        public g(String str) {
            super(str);
        }
    }

    private static com.google.android.gms.internal.d.a a(int i, com.google.android.gms.internal.c.f fVar, com.google.android.gms.internal.d.a[] aVarArr, Set<Integer> set) throws g {
        int i2 = 0;
        if (set.contains(Integer.valueOf(i))) {
            bw("Value cycle detected.  Current value reference: " + i + "." + "  Previous value references: " + set + ".");
        }
        com.google.android.gms.internal.d.a aVar = (com.google.android.gms.internal.d.a) a(fVar.fi, i, "values");
        if (aVarArr[i] != null) {
            return aVarArr[i];
        }
        com.google.android.gms.internal.d.a aVar2 = null;
        set.add(Integer.valueOf(i));
        h h;
        int[] iArr;
        int length;
        int i3;
        int i4;
        switch (aVar.type) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
            case DetectedActivity.TILTING /*5*/:
            case Participant.STATUS_UNRESPONSIVE /*6*/:
            case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                aVar2 = aVar;
                break;
            case DetectedActivity.ON_FOOT /*2*/:
                h = h(aVar);
                aVar2 = g(aVar);
                aVar2.fZ = new com.google.android.gms.internal.d.a[h.fK.length];
                iArr = h.fK;
                length = iArr.length;
                i3 = 0;
                while (i2 < length) {
                    i4 = i3 + 1;
                    aVar2.fZ[i3] = a(iArr[i2], fVar, aVarArr, (Set) set);
                    i2++;
                    i3 = i4;
                }
                break;
            case DetectedActivity.STILL /*3*/:
                aVar2 = g(aVar);
                h h2 = h(aVar);
                if (h2.fL.length != h2.fM.length) {
                    bw("Uneven map keys (" + h2.fL.length + ") and map values (" + h2.fM.length + ")");
                }
                aVar2.ga = new com.google.android.gms.internal.d.a[h2.fL.length];
                aVar2.gb = new com.google.android.gms.internal.d.a[h2.fL.length];
                int[] iArr2 = h2.fL;
                int length2 = iArr2.length;
                i3 = 0;
                i4 = 0;
                while (i3 < length2) {
                    int i5 = i4 + 1;
                    aVar2.ga[i4] = a(iArr2[i3], fVar, aVarArr, (Set) set);
                    i3++;
                    i4 = i5;
                }
                iArr = h2.fM;
                length = iArr.length;
                i3 = 0;
                while (i2 < length) {
                    i4 = i3 + 1;
                    aVar2.gb[i3] = a(iArr[i2], fVar, aVarArr, (Set) set);
                    i2++;
                    i3 = i4;
                }
                break;
            case DetectedActivity.UNKNOWN /*4*/:
                aVar2 = g(aVar);
                aVar2.gc = di.j(a(h(aVar).fP, fVar, aVarArr, (Set) set));
                break;
            case Error.AVS_DECLINE /*7*/:
                aVar2 = g(aVar);
                h = h(aVar);
                aVar2.gg = new com.google.android.gms.internal.d.a[h.fO.length];
                iArr = h.fO;
                length = iArr.length;
                i3 = 0;
                while (i2 < length) {
                    i4 = i3 + 1;
                    aVar2.gg[i3] = a(iArr[i2], fVar, aVarArr, (Set) set);
                    i2++;
                    i3 = i4;
                }
                break;
        }
        if (aVar2 == null) {
            bw("Invalid value: " + aVar);
        }
        aVarArr[i] = aVar2;
        set.remove(Integer.valueOf(i));
        return aVar2;
    }

    private static a a(com.google.android.gms.internal.c.b bVar, com.google.android.gms.internal.c.f fVar, com.google.android.gms.internal.d.a[] aVarArr, int i) throws g {
        b jE = a.jE();
        for (int valueOf : bVar.eS) {
            com.google.android.gms.internal.c.e eVar = (com.google.android.gms.internal.c.e) a(fVar.fj, Integer.valueOf(valueOf).intValue(), "properties");
            String str = (String) a(fVar.fh, eVar.key, "keys");
            com.google.android.gms.internal.d.a aVar = (com.google.android.gms.internal.d.a) a(aVarArr, eVar.value, "values");
            if (com.google.android.gms.internal.b.PUSH_AFTER_EVALUATE.toString().equals(str)) {
                jE.i(aVar);
            } else {
                jE.b(str, aVar);
            }
        }
        return jE.jH();
    }

    private static e a(com.google.android.gms.internal.c.g gVar, List<a> list, List<a> list2, List<a> list3, com.google.android.gms.internal.c.f fVar) {
        f jM = e.jM();
        for (int valueOf : gVar.fy) {
            jM.b((a) list3.get(Integer.valueOf(valueOf).intValue()));
        }
        for (int valueOf2 : gVar.fz) {
            jM.c((a) list3.get(Integer.valueOf(valueOf2).intValue()));
        }
        for (int valueOf22 : gVar.fA) {
            jM.d((a) list.get(Integer.valueOf(valueOf22).intValue()));
        }
        for (int valueOf3 : gVar.fC) {
            jM.by(fVar.fi[Integer.valueOf(valueOf3).intValue()].fY);
        }
        for (int valueOf222 : gVar.fB) {
            jM.e((a) list.get(Integer.valueOf(valueOf222).intValue()));
        }
        for (int valueOf32 : gVar.fD) {
            jM.bz(fVar.fi[Integer.valueOf(valueOf32).intValue()].fY);
        }
        for (int valueOf2222 : gVar.fE) {
            jM.f((a) list2.get(Integer.valueOf(valueOf2222).intValue()));
        }
        for (int valueOf322 : gVar.fG) {
            jM.bA(fVar.fi[Integer.valueOf(valueOf322).intValue()].fY);
        }
        for (int valueOf22222 : gVar.fF) {
            jM.g((a) list2.get(Integer.valueOf(valueOf22222).intValue()));
        }
        for (int valueOf4 : gVar.fH) {
            jM.bB(fVar.fi[Integer.valueOf(valueOf4).intValue()].fY);
        }
        return jM.jX();
    }

    private static <T> T a(T[] tArr, int i, String str) throws g {
        if (i < 0 || i >= tArr.length) {
            bw("Index out of bounds detected: " + i + " in " + str);
        }
        return tArr[i];
    }

    public static c b(com.google.android.gms.internal.c.f fVar) throws g {
        int i;
        int i2 = 0;
        com.google.android.gms.internal.d.a[] aVarArr = new com.google.android.gms.internal.d.a[fVar.fi.length];
        for (i = 0; i < fVar.fi.length; i++) {
            a(i, fVar, aVarArr, new HashSet(0));
        }
        d jI = c.jI();
        List arrayList = new ArrayList();
        for (i = 0; i < fVar.fl.length; i++) {
            arrayList.add(a(fVar.fl[i], fVar, aVarArr, i));
        }
        List arrayList2 = new ArrayList();
        for (i = 0; i < fVar.fm.length; i++) {
            arrayList2.add(a(fVar.fm[i], fVar, aVarArr, i));
        }
        List arrayList3 = new ArrayList();
        for (i = 0; i < fVar.fk.length; i++) {
            a a = a(fVar.fk[i], fVar, aVarArr, i);
            jI.a(a);
            arrayList3.add(a);
        }
        com.google.android.gms.internal.c.g[] gVarArr = fVar.fn;
        int length = gVarArr.length;
        while (i2 < length) {
            jI.a(a(gVarArr[i2], arrayList, arrayList3, arrayList2, fVar));
            i2++;
        }
        jI.bx(fVar.fr);
        jI.bW(fVar.fw);
        return jI.jL();
    }

    public static void b(InputStream inputStream, OutputStream outputStream) throws IOException {
        byte[] bArr = new byte[AccessibilityNodeInfoCompat.ACTION_NEXT_HTML_ELEMENT];
        while (true) {
            int read = inputStream.read(bArr);
            if (read != -1) {
                outputStream.write(bArr, 0, read);
            } else {
                return;
            }
        }
    }

    private static void bw(String str) throws g {
        bh.t(str);
        throw new g(str);
    }

    public static com.google.android.gms.internal.d.a g(com.google.android.gms.internal.d.a aVar) {
        com.google.android.gms.internal.d.a aVar2 = new com.google.android.gms.internal.d.a();
        aVar2.type = aVar.type;
        aVar2.gh = (int[]) aVar.gh.clone();
        if (aVar.gi) {
            aVar2.gi = aVar.gi;
        }
        return aVar2;
    }

    private static h h(com.google.android.gms.internal.d.a aVar) throws g {
        if (((h) aVar.a(h.fI)) == null) {
            bw("Expected a ServingValue and didn't get one. Value is: " + aVar);
        }
        return (h) aVar.a(h.fI);
    }
}
