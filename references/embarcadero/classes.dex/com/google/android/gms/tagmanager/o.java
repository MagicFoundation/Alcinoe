package com.google.android.gms.tagmanager;

import android.content.Context;
import android.os.Looper;
import com.google.android.gms.common.api.Releasable;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.internal.c.j;
import com.google.android.gms.internal.fl;
import com.google.android.gms.internal.fn;

class o extends ca<ContainerHolder> {
    private final String TM;
    private long TR;
    private final TagManager TY;
    private final fl Ty;
    private final d Ub;
    private final cg Uc;
    private final int Ud;
    private f Ue;
    private volatile n Uf;
    private volatile boolean Ug;
    private j Uh;
    private String Ui;
    private e Uj;
    private a Uk;
    private final Context mContext;
    private final Looper zs;

    interface a {
        boolean b(Container container);
    }

    /* renamed from: com.google.android.gms.tagmanager.o.2 */
    class AnonymousClass2 implements a {
        final /* synthetic */ o Ul;
        final /* synthetic */ boolean Um;

        AnonymousClass2(o oVar, boolean z) {
            this.Ul = oVar;
            this.Um = z;
        }

        public boolean b(Container container) {
            return this.Um ? container.getLastRefreshTime() + 43200000 >= this.Ul.Ty.currentTimeMillis() : !container.isDefault();
        }
    }

    private class b implements bg<com.google.android.gms.internal.jd.a> {
        final /* synthetic */ o Ul;

        private b(o oVar) {
            this.Ul = oVar;
        }

        public void a(com.google.android.gms.internal.jd.a aVar) {
            j jVar;
            if (aVar.Yc != null) {
                jVar = aVar.Yc;
            } else {
                com.google.android.gms.internal.c.f fVar = aVar.fV;
                jVar = new j();
                jVar.fV = fVar;
                jVar.fU = null;
                jVar.fW = fVar.fr;
            }
            this.Ul.a(jVar, aVar.Yb, true);
        }

        public void a(com.google.android.gms.tagmanager.bg.a aVar) {
            if (!this.Ul.Ug) {
                this.Ul.s(0);
            }
        }

        public /* synthetic */ void i(Object obj) {
            a((com.google.android.gms.internal.jd.a) obj);
        }

        public void iM() {
        }
    }

    private class c implements bg<j> {
        final /* synthetic */ o Ul;

        private c(o oVar) {
            this.Ul = oVar;
        }

        public void a(com.google.android.gms.tagmanager.bg.a aVar) {
            if (this.Ul.Uf != null) {
                this.Ul.a(this.Ul.Uf);
            } else {
                this.Ul.a(this.Ul.O(Status.zS));
            }
            this.Ul.s(3600000);
        }

        public void b(j jVar) {
            synchronized (this.Ul) {
                if (jVar.fV == null) {
                    if (this.Ul.Uh.fV == null) {
                        bh.t("Current resource is null; network resource is also null");
                        this.Ul.s(3600000);
                        return;
                    }
                    jVar.fV = this.Ul.Uh.fV;
                }
                this.Ul.a(jVar, this.Ul.Ty.currentTimeMillis(), false);
                bh.v("setting refresh time to current time: " + this.Ul.TR);
                if (!this.Ul.iL()) {
                    this.Ul.a(jVar);
                }
            }
        }

        public /* synthetic */ void i(Object obj) {
            b((j) obj);
        }

        public void iM() {
        }
    }

    private class d implements com.google.android.gms.tagmanager.n.a {
        final /* synthetic */ o Ul;

        private d(o oVar) {
            this.Ul = oVar;
        }

        public void bc(String str) {
            this.Ul.bc(str);
        }

        public String iF() {
            return this.Ul.iF();
        }

        public void iH() {
            if (this.Ul.Uc.cl()) {
                this.Ul.s(0);
            }
        }
    }

    interface e extends Releasable {
        void a(bg<j> bgVar);

        void bf(String str);

        void d(long j, String str);
    }

    interface f extends Releasable {
        void a(bg<com.google.android.gms.internal.jd.a> bgVar);

        void b(com.google.android.gms.internal.jd.a aVar);

        com.google.android.gms.tagmanager.cr.c bP(int i);

        void iN();
    }

    o(Context context, TagManager tagManager, Looper looper, String str, int i, f fVar, e eVar, fl flVar, cg cgVar) {
        super(looper == null ? Looper.getMainLooper() : looper);
        this.mContext = context;
        this.TY = tagManager;
        if (looper == null) {
            looper = Looper.getMainLooper();
        }
        this.zs = looper;
        this.TM = str;
        this.Ud = i;
        this.Ue = fVar;
        this.Uj = eVar;
        this.Ub = new d();
        this.Uh = new j();
        this.Ty = flVar;
        this.Uc = cgVar;
        if (iL()) {
            bc(ce.ju().jw());
        }
    }

    public o(Context context, TagManager tagManager, Looper looper, String str, int i, r rVar) {
        this(context, tagManager, looper, str, i, new cq(context, str), new cp(context, str, rVar), fn.eI(), new bf(30, 900000, 5000, "refreshing", fn.eI()));
    }

    private synchronized void a(j jVar) {
        if (this.Ue != null) {
            com.google.android.gms.internal.jd.a aVar = new com.google.android.gms.internal.jd.a();
            aVar.Yb = this.TR;
            aVar.fV = new com.google.android.gms.internal.c.f();
            aVar.Yc = jVar;
            this.Ue.b(aVar);
        }
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    private synchronized void a(j jVar, long j, boolean z) {
        Container container;
        if (z) {
        }
        if (!isReady() || this.Uf == null) {
            this.Uh = jVar;
            this.TR = j;
            s(Math.max(0, Math.min(43200000, (this.TR + 43200000) - this.Ty.currentTimeMillis())));
            container = new Container(this.mContext, this.TY.getDataLayer(), this.TM, j, jVar);
        } else {
            this.Uh = jVar;
            this.TR = j;
            s(Math.max(0, Math.min(43200000, (this.TR + 43200000) - this.Ty.currentTimeMillis())));
            container = new Container(this.mContext, this.TY.getDataLayer(), this.TM, j, jVar);
        }
        if (this.Uf == null) {
            this.Uf = new n(this.TY, this.zs, container, this.Ub);
        } else {
            this.Uf.a(container);
        }
        if (!isReady() && this.Uk.b(container)) {
            a(this.Uf);
        }
    }

    private boolean iL() {
        ce ju = ce.ju();
        return (ju.jv() == a.CONTAINER || ju.jv() == a.CONTAINER_DEBUG) && this.TM.equals(ju.getContainerId());
    }

    private synchronized void s(long j) {
        if (this.Uj == null) {
            bh.w("Refresh requested, but no network load scheduler.");
        } else {
            this.Uj.d(j, this.Uh.fW);
        }
    }

    private void z(boolean z) {
        this.Ue.a(new b());
        this.Uj.a(new c());
        com.google.android.gms.tagmanager.cr.c bP = this.Ue.bP(this.Ud);
        if (bP != null) {
            this.Uf = new n(this.TY, this.zs, new Container(this.mContext, this.TY.getDataLayer(), this.TM, 0, bP), this.Ub);
        }
        this.Uk = new AnonymousClass2(this, z);
        if (iL()) {
            this.Uj.d(0, "");
        } else {
            this.Ue.iN();
        }
    }

    protected ContainerHolder O(Status status) {
        if (this.Uf != null) {
            return this.Uf;
        }
        if (status == Status.zS) {
            bh.t("timer expired: setting result to failure");
        }
        return new n(status);
    }

    synchronized void bc(String str) {
        this.Ui = str;
        if (this.Uj != null) {
            this.Uj.bf(str);
        }
    }

    protected /* synthetic */ Result d(Status status) {
        return O(status);
    }

    synchronized String iF() {
        return this.Ui;
    }

    public void iI() {
        com.google.android.gms.tagmanager.cr.c bP = this.Ue.bP(this.Ud);
        if (bP != null) {
            a(new n(this.TY, this.zs, new Container(this.mContext, this.TY.getDataLayer(), this.TM, 0, bP), new com.google.android.gms.tagmanager.n.a() {
                final /* synthetic */ o Ul;

                {
                    this.Ul = r1;
                }

                public void bc(String str) {
                    this.Ul.bc(str);
                }

                public String iF() {
                    return this.Ul.iF();
                }

                public void iH() {
                    bh.w("Refresh ignored: container loaded as default only.");
                }
            }));
        } else {
            String str = "Default was requested, but no default container was found";
            bh.t(str);
            a(O(new Status(10, str, null)));
        }
        this.Uj = null;
        this.Ue = null;
    }

    public void iJ() {
        z(false);
    }

    public void iK() {
        z(true);
    }
}
