package com.google.android.gms.internal;

import java.lang.ref.WeakReference;

public final class w {
    private final Runnable kW;
    private z kX;
    private boolean kY;

    /* renamed from: com.google.android.gms.internal.w.1 */
    class AnonymousClass1 implements Runnable {
        private final WeakReference<v> kZ;
        final /* synthetic */ v la;
        final /* synthetic */ w lb;

        AnonymousClass1(w wVar, v vVar) {
            this.lb = wVar;
            this.la = vVar;
            this.kZ = new WeakReference(this.la);
        }

        public void run() {
            this.lb.kY = false;
            v vVar = (v) this.kZ.get();
            if (vVar != null) {
                vVar.b(this.lb.kX);
            }
        }
    }

    public w(v vVar) {
        this.kY = false;
        this.kW = new AnonymousClass1(this, vVar);
    }

    public void a(z zVar, long j) {
        if (this.kY) {
            da.w("An ad refresh is already scheduled.");
            return;
        }
        da.u("Scheduling ad refresh " + j + " milliseconds from now.");
        this.kX = zVar;
        this.kY = true;
        cz.pT.postDelayed(this.kW, j);
    }

    public void cancel() {
        cz.pT.removeCallbacks(this.kW);
    }

    public void d(z zVar) {
        a(zVar, 60000);
    }
}
