package com.google.android.gms.tagmanager;

import android.content.Context;
import android.support.v4.view.accessibility.AccessibilityEventCompat;
import com.google.android.gms.internal.c.i;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.tagmanager.cr.e;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

class ct {
    private static final by<com.google.android.gms.internal.d.a> WH;
    private final DataLayer TN;
    private final com.google.android.gms.tagmanager.cr.c WI;
    private final ag WJ;
    private final Map<String, aj> WK;
    private final Map<String, aj> WL;
    private final Map<String, aj> WM;
    private final k<com.google.android.gms.tagmanager.cr.a, by<com.google.android.gms.internal.d.a>> WN;
    private final k<String, b> WO;
    private final Set<e> WP;
    private final Map<String, c> WQ;
    private volatile String WR;
    private int WS;

    interface a {
        void a(e eVar, Set<com.google.android.gms.tagmanager.cr.a> set, Set<com.google.android.gms.tagmanager.cr.a> set2, cn cnVar);
    }

    private static class b {
        private by<com.google.android.gms.internal.d.a> WY;
        private com.google.android.gms.internal.d.a Wt;

        public b(by<com.google.android.gms.internal.d.a> byVar, com.google.android.gms.internal.d.a aVar) {
            this.WY = byVar;
            this.Wt = aVar;
        }

        public int getSize() {
            return (this.Wt == null ? 0 : this.Wt.eW()) + ((com.google.android.gms.internal.d.a) this.WY.getObject()).eW();
        }

        public com.google.android.gms.internal.d.a jG() {
            return this.Wt;
        }

        public by<com.google.android.gms.internal.d.a> ka() {
            return this.WY;
        }
    }

    private static class c {
        private final Set<e> WP;
        private final Map<e, List<com.google.android.gms.tagmanager.cr.a>> WZ;
        private final Map<e, List<com.google.android.gms.tagmanager.cr.a>> Xa;
        private final Map<e, List<String>> Xb;
        private final Map<e, List<String>> Xc;
        private com.google.android.gms.tagmanager.cr.a Xd;

        public c() {
            this.WP = new HashSet();
            this.WZ = new HashMap();
            this.Xb = new HashMap();
            this.Xa = new HashMap();
            this.Xc = new HashMap();
        }

        public void a(e eVar, com.google.android.gms.tagmanager.cr.a aVar) {
            List list = (List) this.WZ.get(eVar);
            if (list == null) {
                list = new ArrayList();
                this.WZ.put(eVar, list);
            }
            list.add(aVar);
        }

        public void a(e eVar, String str) {
            List list = (List) this.Xb.get(eVar);
            if (list == null) {
                list = new ArrayList();
                this.Xb.put(eVar, list);
            }
            list.add(str);
        }

        public void b(e eVar) {
            this.WP.add(eVar);
        }

        public void b(e eVar, com.google.android.gms.tagmanager.cr.a aVar) {
            List list = (List) this.Xa.get(eVar);
            if (list == null) {
                list = new ArrayList();
                this.Xa.put(eVar, list);
            }
            list.add(aVar);
        }

        public void b(e eVar, String str) {
            List list = (List) this.Xc.get(eVar);
            if (list == null) {
                list = new ArrayList();
                this.Xc.put(eVar, list);
            }
            list.add(str);
        }

        public void i(com.google.android.gms.tagmanager.cr.a aVar) {
            this.Xd = aVar;
        }

        public Set<e> kb() {
            return this.WP;
        }

        public Map<e, List<com.google.android.gms.tagmanager.cr.a>> kc() {
            return this.WZ;
        }

        public Map<e, List<String>> kd() {
            return this.Xb;
        }

        public Map<e, List<String>> ke() {
            return this.Xc;
        }

        public Map<e, List<com.google.android.gms.tagmanager.cr.a>> kf() {
            return this.Xa;
        }

        public com.google.android.gms.tagmanager.cr.a kg() {
            return this.Xd;
        }
    }

    /* renamed from: com.google.android.gms.tagmanager.ct.3 */
    class AnonymousClass3 implements a {
        final /* synthetic */ ct WT;
        final /* synthetic */ Map WU;
        final /* synthetic */ Map WV;
        final /* synthetic */ Map WW;
        final /* synthetic */ Map WX;

        AnonymousClass3(ct ctVar, Map map, Map map2, Map map3, Map map4) {
            this.WT = ctVar;
            this.WU = map;
            this.WV = map2;
            this.WW = map3;
            this.WX = map4;
        }

        public void a(e eVar, Set<com.google.android.gms.tagmanager.cr.a> set, Set<com.google.android.gms.tagmanager.cr.a> set2, cn cnVar) {
            List list = (List) this.WU.get(eVar);
            List list2 = (List) this.WV.get(eVar);
            if (list != null) {
                set.addAll(list);
                cnVar.jl().b(list, list2);
            }
            list = (List) this.WW.get(eVar);
            list2 = (List) this.WX.get(eVar);
            if (list != null) {
                set2.addAll(list);
                cnVar.jm().b(list, list2);
            }
        }
    }

    static {
        WH = new by(di.ku(), true);
    }

    public ct(Context context, com.google.android.gms.tagmanager.cr.c cVar, DataLayer dataLayer, com.google.android.gms.tagmanager.s.a aVar, com.google.android.gms.tagmanager.s.a aVar2, ag agVar) {
        if (cVar == null) {
            throw new NullPointerException("resource cannot be null");
        }
        this.WI = cVar;
        this.WP = new HashSet(cVar.jJ());
        this.TN = dataLayer;
        this.WJ = agVar;
        this.WN = new l().a(AccessibilityEventCompat.TYPE_TOUCH_INTERACTION_START, new com.google.android.gms.tagmanager.l.a<com.google.android.gms.tagmanager.cr.a, by<com.google.android.gms.internal.d.a>>() {
            final /* synthetic */ ct WT;

            {
                this.WT = r1;
            }

            public int a(com.google.android.gms.tagmanager.cr.a aVar, by<com.google.android.gms.internal.d.a> byVar) {
                return ((com.google.android.gms.internal.d.a) byVar.getObject()).eW();
            }

            public /* synthetic */ int sizeOf(Object x0, Object x1) {
                return a((com.google.android.gms.tagmanager.cr.a) x0, (by) x1);
            }
        });
        this.WO = new l().a(AccessibilityEventCompat.TYPE_TOUCH_INTERACTION_START, new com.google.android.gms.tagmanager.l.a<String, b>() {
            final /* synthetic */ ct WT;

            {
                this.WT = r1;
            }

            public int a(String str, b bVar) {
                return str.length() + bVar.getSize();
            }

            public /* synthetic */ int sizeOf(Object x0, Object x1) {
                return a((String) x0, (b) x1);
            }
        });
        this.WK = new HashMap();
        b(new i(context));
        b(new s(aVar2));
        b(new w(dataLayer));
        b(new dj(context, dataLayer));
        this.WL = new HashMap();
        c(new q());
        c(new ad());
        c(new ae());
        c(new al());
        c(new am());
        c(new bd());
        c(new be());
        c(new ci());
        c(new dc());
        this.WM = new HashMap();
        a(new b(context));
        a(new c(context));
        a(new e(context));
        a(new f(context));
        a(new g(context));
        a(new h(context));
        a(new m());
        a(new p(this.WI.getVersion()));
        a(new s(aVar));
        a(new u(dataLayer));
        a(new z(context));
        a(new aa());
        a(new ac());
        a(new ah(this));
        a(new an());
        a(new ao());
        a(new ax(context));
        a(new az());
        a(new bc());
        a(new bk(context));
        a(new bz());
        a(new cc());
        a(new cf());
        a(new ch());
        a(new cj(context));
        a(new cu());
        a(new cv());
        a(new de());
        this.WQ = new HashMap();
        for (e eVar : this.WP) {
            if (agVar.jb()) {
                a(eVar.jR(), eVar.jS(), "add macro");
                a(eVar.jW(), eVar.jT(), "remove macro");
                a(eVar.jP(), eVar.jU(), "add tag");
                a(eVar.jQ(), eVar.jV(), "remove tag");
            }
            int i = 0;
            while (i < eVar.jR().size()) {
                com.google.android.gms.tagmanager.cr.a aVar3 = (com.google.android.gms.tagmanager.cr.a) eVar.jR().get(i);
                String str = "Unknown";
                if (agVar.jb() && i < eVar.jS().size()) {
                    str = (String) eVar.jS().get(i);
                }
                c c = c(this.WQ, h(aVar3));
                c.b(eVar);
                c.a(eVar, aVar3);
                c.a(eVar, str);
                i++;
            }
            i = 0;
            while (i < eVar.jW().size()) {
                aVar3 = (com.google.android.gms.tagmanager.cr.a) eVar.jW().get(i);
                str = "Unknown";
                if (agVar.jb() && i < eVar.jT().size()) {
                    str = (String) eVar.jT().get(i);
                }
                c = c(this.WQ, h(aVar3));
                c.b(eVar);
                c.b(eVar, aVar3);
                c.b(eVar, str);
                i++;
            }
        }
        for (Entry entry : this.WI.jK().entrySet()) {
            for (com.google.android.gms.tagmanager.cr.a aVar32 : (List) entry.getValue()) {
                if (!di.n((com.google.android.gms.internal.d.a) aVar32.jF().get(com.google.android.gms.internal.b.NOT_DEFAULT_MACRO.toString())).booleanValue()) {
                    c(this.WQ, (String) entry.getKey()).i(aVar32);
                }
            }
        }
    }

    private by<com.google.android.gms.internal.d.a> a(com.google.android.gms.internal.d.a aVar, Set<String> set, dk dkVar) {
        if (!aVar.gi) {
            return new by(aVar, true);
        }
        com.google.android.gms.internal.d.a g;
        int i;
        by a;
        switch (aVar.type) {
            case DetectedActivity.ON_FOOT /*2*/:
                g = cr.g(aVar);
                g.fZ = new com.google.android.gms.internal.d.a[aVar.fZ.length];
                for (i = 0; i < aVar.fZ.length; i++) {
                    a = a(aVar.fZ[i], (Set) set, dkVar.bS(i));
                    if (a == WH) {
                        return WH;
                    }
                    g.fZ[i] = (com.google.android.gms.internal.d.a) a.getObject();
                }
                return new by(g, false);
            case DetectedActivity.STILL /*3*/:
                g = cr.g(aVar);
                if (aVar.ga.length != aVar.gb.length) {
                    bh.t("Invalid serving value: " + aVar.toString());
                    return WH;
                }
                g.ga = new com.google.android.gms.internal.d.a[aVar.ga.length];
                g.gb = new com.google.android.gms.internal.d.a[aVar.ga.length];
                for (i = 0; i < aVar.ga.length; i++) {
                    a = a(aVar.ga[i], (Set) set, dkVar.bT(i));
                    by a2 = a(aVar.gb[i], (Set) set, dkVar.bU(i));
                    if (a == WH || a2 == WH) {
                        return WH;
                    }
                    g.ga[i] = (com.google.android.gms.internal.d.a) a.getObject();
                    g.gb[i] = (com.google.android.gms.internal.d.a) a2.getObject();
                }
                return new by(g, false);
            case DetectedActivity.UNKNOWN /*4*/:
                if (set.contains(aVar.gc)) {
                    bh.t("Macro cycle detected.  Current macro reference: " + aVar.gc + "." + "  Previous macro references: " + set.toString() + ".");
                    return WH;
                }
                set.add(aVar.gc);
                by<com.google.android.gms.internal.d.a> a3 = dl.a(a(aVar.gc, (Set) set, dkVar.jq()), aVar.gh);
                set.remove(aVar.gc);
                return a3;
            case Error.AVS_DECLINE /*7*/:
                g = cr.g(aVar);
                g.gg = new com.google.android.gms.internal.d.a[aVar.gg.length];
                for (i = 0; i < aVar.gg.length; i++) {
                    a = a(aVar.gg[i], (Set) set, dkVar.bV(i));
                    if (a == WH) {
                        return WH;
                    }
                    g.gg[i] = (com.google.android.gms.internal.d.a) a.getObject();
                }
                return new by(g, false);
            default:
                bh.t("Unknown type: " + aVar.type);
                return WH;
        }
    }

    private by<com.google.android.gms.internal.d.a> a(String str, Set<String> set, bj bjVar) {
        this.WS++;
        b bVar = (b) this.WO.get(str);
        if (bVar == null || this.WJ.jb()) {
            c cVar = (c) this.WQ.get(str);
            if (cVar == null) {
                bh.t(jZ() + "Invalid macro: " + str);
                this.WS--;
                return WH;
            }
            com.google.android.gms.tagmanager.cr.a kg;
            by a = a(str, cVar.kb(), cVar.kc(), cVar.kd(), cVar.kf(), cVar.ke(), set, bjVar.iS());
            if (((Set) a.getObject()).isEmpty()) {
                kg = cVar.kg();
            } else {
                if (((Set) a.getObject()).size() > 1) {
                    bh.w(jZ() + "Multiple macros active for macroName " + str);
                }
                kg = (com.google.android.gms.tagmanager.cr.a) ((Set) a.getObject()).iterator().next();
            }
            if (kg == null) {
                this.WS--;
                return WH;
            }
            by a2 = a(this.WM, kg, (Set) set, bjVar.jh());
            boolean z = a.jr() && a2.jr();
            by<com.google.android.gms.internal.d.a> byVar = a2 == WH ? WH : new by(a2.getObject(), z);
            com.google.android.gms.internal.d.a jG = kg.jG();
            if (byVar.jr()) {
                this.WO.e(str, new b(byVar, jG));
            }
            a(jG, (Set) set);
            this.WS--;
            return byVar;
        }
        a(bVar.jG(), (Set) set);
        this.WS--;
        return bVar.ka();
    }

    private by<com.google.android.gms.internal.d.a> a(Map<String, aj> map, com.google.android.gms.tagmanager.cr.a aVar, Set<String> set, ck ckVar) {
        boolean z = true;
        com.google.android.gms.internal.d.a aVar2 = (com.google.android.gms.internal.d.a) aVar.jF().get(com.google.android.gms.internal.b.FUNCTION.toString());
        if (aVar2 == null) {
            bh.t("No function id in properties");
            return WH;
        }
        String str = aVar2.gd;
        aj ajVar = (aj) map.get(str);
        if (ajVar == null) {
            bh.t(str + " has no backing implementation.");
            return WH;
        }
        by<com.google.android.gms.internal.d.a> byVar = (by) this.WN.get(aVar);
        if (byVar != null && !this.WJ.jb()) {
            return byVar;
        }
        Map hashMap = new HashMap();
        boolean z2 = true;
        for (Entry entry : aVar.jF().entrySet()) {
            by a = a((com.google.android.gms.internal.d.a) entry.getValue(), (Set) set, ckVar.bs((String) entry.getKey()).e((com.google.android.gms.internal.d.a) entry.getValue()));
            if (a == WH) {
                return WH;
            }
            boolean z3;
            if (a.jr()) {
                aVar.a((String) entry.getKey(), (com.google.android.gms.internal.d.a) a.getObject());
                z3 = z2;
            } else {
                z3 = false;
            }
            hashMap.put(entry.getKey(), a.getObject());
            z2 = z3;
        }
        if (ajVar.a(hashMap.keySet())) {
            if (!(z2 && ajVar.iy())) {
                z = false;
            }
            byVar = new by(ajVar.u(hashMap), z);
            if (z) {
                this.WN.e(aVar, byVar);
            }
            ckVar.d((com.google.android.gms.internal.d.a) byVar.getObject());
            return byVar;
        }
        bh.t("Incorrect keys for function " + str + " required " + ajVar.jd() + " had " + hashMap.keySet());
        return WH;
    }

    private by<Set<com.google.android.gms.tagmanager.cr.a>> a(Set<e> set, Set<String> set2, a aVar, cs csVar) {
        Set hashSet = new HashSet();
        Collection hashSet2 = new HashSet();
        boolean z = true;
        for (e eVar : set) {
            cn jp = csVar.jp();
            by a = a(eVar, (Set) set2, jp);
            if (((Boolean) a.getObject()).booleanValue()) {
                aVar.a(eVar, hashSet, hashSet2, jp);
            }
            boolean z2 = z && a.jr();
            z = z2;
        }
        hashSet.removeAll(hashSet2);
        csVar.b(hashSet);
        return new by(hashSet, z);
    }

    private void a(com.google.android.gms.internal.d.a aVar, Set<String> set) {
        if (aVar != null) {
            by a = a(aVar, (Set) set, new bw());
            if (a != WH) {
                Object o = di.o((com.google.android.gms.internal.d.a) a.getObject());
                if (o instanceof Map) {
                    this.TN.push((Map) o);
                } else if (o instanceof List) {
                    for (Object o2 : (List) o2) {
                        if (o2 instanceof Map) {
                            this.TN.push((Map) o2);
                        } else {
                            bh.w("pushAfterEvaluate: value not a Map");
                        }
                    }
                } else {
                    bh.w("pushAfterEvaluate: value not a Map or List");
                }
            }
        }
    }

    private static void a(List<com.google.android.gms.tagmanager.cr.a> list, List<String> list2, String str) {
        if (list.size() != list2.size()) {
            bh.u("Invalid resource: imbalance of rule names of functions for " + str + " operation. Using default rule name instead");
        }
    }

    private static void a(Map<String, aj> map, aj ajVar) {
        if (map.containsKey(ajVar.jc())) {
            throw new IllegalArgumentException("Duplicate function type name: " + ajVar.jc());
        }
        map.put(ajVar.jc(), ajVar);
    }

    private static c c(Map<String, c> map, String str) {
        c cVar = (c) map.get(str);
        if (cVar != null) {
            return cVar;
        }
        cVar = new c();
        map.put(str, cVar);
        return cVar;
    }

    private static String h(com.google.android.gms.tagmanager.cr.a aVar) {
        return di.j((com.google.android.gms.internal.d.a) aVar.jF().get(com.google.android.gms.internal.b.INSTANCE_NAME.toString()));
    }

    private String jZ() {
        if (this.WS <= 1) {
            return "";
        }
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(Integer.toString(this.WS));
        for (int i = 2; i < this.WS; i++) {
            stringBuilder.append(' ');
        }
        stringBuilder.append(": ");
        return stringBuilder.toString();
    }

    by<Boolean> a(com.google.android.gms.tagmanager.cr.a aVar, Set<String> set, ck ckVar) {
        by a = a(this.WL, aVar, (Set) set, ckVar);
        Boolean n = di.n((com.google.android.gms.internal.d.a) a.getObject());
        ckVar.d(di.r(n));
        return new by(n, a.jr());
    }

    by<Boolean> a(e eVar, Set<String> set, cn cnVar) {
        boolean z = true;
        for (com.google.android.gms.tagmanager.cr.a a : eVar.jO()) {
            by a2 = a(a, (Set) set, cnVar.jj());
            if (((Boolean) a2.getObject()).booleanValue()) {
                cnVar.f(di.r(Boolean.valueOf(false)));
                return new by(Boolean.valueOf(false), a2.jr());
            }
            boolean z2 = z && a2.jr();
            z = z2;
        }
        for (com.google.android.gms.tagmanager.cr.a a3 : eVar.jN()) {
            a2 = a(a3, (Set) set, cnVar.jk());
            if (((Boolean) a2.getObject()).booleanValue()) {
                z = z && a2.jr();
            } else {
                cnVar.f(di.r(Boolean.valueOf(false)));
                return new by(Boolean.valueOf(false), a2.jr());
            }
        }
        cnVar.f(di.r(Boolean.valueOf(true)));
        return new by(Boolean.valueOf(true), z);
    }

    by<Set<com.google.android.gms.tagmanager.cr.a>> a(String str, Set<e> set, Map<e, List<com.google.android.gms.tagmanager.cr.a>> map, Map<e, List<String>> map2, Map<e, List<com.google.android.gms.tagmanager.cr.a>> map3, Map<e, List<String>> map4, Set<String> set2, cs csVar) {
        return a((Set) set, (Set) set2, new AnonymousClass3(this, map, map2, map3, map4), csVar);
    }

    by<Set<com.google.android.gms.tagmanager.cr.a>> a(Set<e> set, cs csVar) {
        return a((Set) set, new HashSet(), new a() {
            final /* synthetic */ ct WT;

            {
                this.WT = r1;
            }

            public void a(e eVar, Set<com.google.android.gms.tagmanager.cr.a> set, Set<com.google.android.gms.tagmanager.cr.a> set2, cn cnVar) {
                set.addAll(eVar.jP());
                set2.addAll(eVar.jQ());
                cnVar.jn().b(eVar.jP(), eVar.jU());
                cnVar.jo().b(eVar.jQ(), eVar.jV());
            }
        }, csVar);
    }

    void a(aj ajVar) {
        a(this.WM, ajVar);
    }

    void b(aj ajVar) {
        a(this.WK, ajVar);
    }

    public by<com.google.android.gms.internal.d.a> bC(String str) {
        this.WS = 0;
        af bl = this.WJ.bl(str);
        by<com.google.android.gms.internal.d.a> a = a(str, new HashSet(), bl.iY());
        bl.ja();
        return a;
    }

    synchronized void bD(String str) {
        this.WR = str;
    }

    public synchronized void ba(String str) {
        bD(str);
        af bm = this.WJ.bm(str);
        t iZ = bm.iZ();
        for (com.google.android.gms.tagmanager.cr.a a : (Set) a(this.WP, iZ.iS()).getObject()) {
            a(this.WK, a, new HashSet(), iZ.iR());
        }
        bm.ja();
        bD(null);
    }

    void c(aj ajVar) {
        a(this.WL, ajVar);
    }

    public synchronized void f(List<i> list) {
        for (i iVar : list) {
            if (iVar.name == null || !iVar.name.startsWith("gaExperiment:")) {
                bh.v("Ignored supplemental: " + iVar);
            } else {
                ai.a(this.TN, iVar);
            }
        }
    }

    synchronized String jY() {
        return this.WR;
    }
}
