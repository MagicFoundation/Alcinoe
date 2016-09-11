package com.google.android.gms.internal;

import android.content.Context;
import android.os.SystemClock;
import com.google.android.gms.dynamic.c;
import com.google.android.gms.internal.bc.a;

public final class bb implements a {
    private final bf kH;
    private final z kX;
    private final String mC;
    private final Context mContext;
    private final long mD;
    private final ax mE;
    private final ab mF;
    private final db mG;
    private bg mH;
    private int mI;
    private final Object mg;

    /* renamed from: com.google.android.gms.internal.bb.1 */
    class AnonymousClass1 implements Runnable {
        final /* synthetic */ ba mJ;
        final /* synthetic */ bb mK;

        AnonymousClass1(bb bbVar, ba baVar) {
            this.mK = bbVar;
            this.mJ = baVar;
        }

        public void run() {
            synchronized (this.mK.mg) {
                if (this.mK.mI != -2) {
                    return;
                }
                this.mK.mH = this.mK.ao();
                if (this.mK.mH == null) {
                    this.mK.f(4);
                    return;
                }
                this.mJ.a(this.mK);
                this.mK.a(this.mJ);
            }
        }
    }

    public bb(Context context, String str, bf bfVar, ay ayVar, ax axVar, z zVar, ab abVar, db dbVar) {
        this.mg = new Object();
        this.mI = -2;
        this.mContext = context;
        this.mC = str;
        this.kH = bfVar;
        this.mD = ayVar.ms != -1 ? ayVar.ms : 10000;
        this.mE = axVar;
        this.kX = zVar;
        this.mF = abVar;
        this.mG = dbVar;
    }

    private void a(long j, long j2, long j3, long j4) {
        while (this.mI == -2) {
            b(j, j2, j3, j4);
        }
    }

    private void a(ba baVar) {
        try {
            if (this.mG.pW < 4100000) {
                if (this.mF.lo) {
                    this.mH.a(c.h(this.mContext), this.kX, this.mE.mq, baVar);
                } else {
                    this.mH.a(c.h(this.mContext), this.mF, this.kX, this.mE.mq, (bh) baVar);
                }
            } else if (this.mF.lo) {
                this.mH.a(c.h(this.mContext), this.kX, this.mE.mq, this.mE.adJson, (bh) baVar);
            } else {
                this.mH.a(c.h(this.mContext), this.mF, this.kX, this.mE.mq, this.mE.adJson, baVar);
            }
        } catch (Throwable e) {
            da.b("Could not request ad from mediation adapter.", e);
            f(5);
        }
    }

    private bg ao() {
        da.u("Instantiating mediation adapter: " + this.mC);
        try {
            return this.kH.m(this.mC);
        } catch (Throwable e) {
            da.a("Could not instantiate mediation adapter: " + this.mC, e);
            return null;
        }
    }

    private void b(long j, long j2, long j3, long j4) {
        long elapsedRealtime = SystemClock.elapsedRealtime();
        long j5 = j2 - (elapsedRealtime - j);
        elapsedRealtime = j4 - (elapsedRealtime - j3);
        if (j5 <= 0 || elapsedRealtime <= 0) {
            da.u("Timed out waiting for adapter.");
            this.mI = 3;
            return;
        }
        try {
            this.mg.wait(Math.min(j5, elapsedRealtime));
        } catch (InterruptedException e) {
            this.mI = -1;
        }
    }

    public bc b(long j, long j2) {
        bc bcVar;
        synchronized (this.mg) {
            long elapsedRealtime = SystemClock.elapsedRealtime();
            ba baVar = new ba();
            cz.pT.post(new AnonymousClass1(this, baVar));
            a(elapsedRealtime, this.mD, j, j2);
            bcVar = new bc(this.mE, this.mH, this.mC, baVar, this.mI);
        }
        return bcVar;
    }

    public void cancel() {
        synchronized (this.mg) {
            try {
                if (this.mH != null) {
                    this.mH.destroy();
                }
            } catch (Throwable e) {
                da.b("Could not destroy mediation adapter.", e);
            }
            this.mI = -1;
            this.mg.notify();
        }
    }

    public void f(int i) {
        synchronized (this.mg) {
            this.mI = i;
            this.mg.notify();
        }
    }
}
