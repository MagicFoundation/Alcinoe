package com.google.android.gms.tagmanager;

import android.content.Context;
import com.google.android.gms.internal.c.j;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

class cp implements e {
    private final String TM;
    private String Ui;
    private bg<j> Wi;
    private r Wj;
    private final ScheduledExecutorService Wl;
    private final a Wm;
    private ScheduledFuture<?> Wn;
    private boolean mClosed;
    private final Context mContext;

    interface a {
        co a(r rVar);
    }

    interface b {
        ScheduledExecutorService jB();
    }

    public cp(Context context, String str, r rVar) {
        this(context, str, rVar, null, null);
    }

    cp(Context context, String str, r rVar, b bVar, a aVar) {
        this.Wj = rVar;
        this.mContext = context;
        this.TM = str;
        if (bVar == null) {
            bVar = new b() {
                final /* synthetic */ cp Wo;

                {
                    this.Wo = r1;
                }

                public ScheduledExecutorService jB() {
                    return Executors.newSingleThreadScheduledExecutor();
                }
            };
        }
        this.Wl = bVar.jB();
        if (aVar == null) {
            this.Wm = new a() {
                final /* synthetic */ cp Wo;

                {
                    this.Wo = r1;
                }

                public co a(r rVar) {
                    return new co(this.Wo.mContext, this.Wo.TM, rVar);
                }
            };
        } else {
            this.Wm = aVar;
        }
    }

    private co bv(String str) {
        co a = this.Wm.a(this.Wj);
        a.a(this.Wi);
        a.bf(this.Ui);
        a.bu(str);
        return a;
    }

    private synchronized void jA() {
        if (this.mClosed) {
            throw new IllegalStateException("called method after closed");
        }
    }

    public synchronized void a(bg<j> bgVar) {
        jA();
        this.Wi = bgVar;
    }

    public synchronized void bf(String str) {
        jA();
        this.Ui = str;
    }

    public synchronized void d(long j, String str) {
        bh.v("loadAfterDelay: containerId=" + this.TM + " delay=" + j);
        jA();
        if (this.Wi == null) {
            throw new IllegalStateException("callback must be set before loadAfterDelay() is called.");
        }
        if (this.Wn != null) {
            this.Wn.cancel(false);
        }
        this.Wn = this.Wl.schedule(bv(str), j, TimeUnit.MILLISECONDS);
    }

    public synchronized void release() {
        jA();
        if (this.Wn != null) {
            this.Wn.cancel(false);
        }
        this.Wl.shutdown();
        this.mClosed = true;
    }
}
