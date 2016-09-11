package com.google.android.gms.tagmanager;

import android.content.Context;
import android.os.Handler;
import android.os.Handler.Callback;
import android.os.Message;

class cy extends cx {
    private static cy Xp;
    private static final Object ri;
    private Context Xf;
    private at Xg;
    private volatile ar Xh;
    private int Xi;
    private boolean Xj;
    private boolean Xk;
    private boolean Xl;
    private au Xm;
    private bn Xn;
    private boolean Xo;
    private boolean connected;
    private Handler handler;

    static {
        ri = new Object();
    }

    private cy() {
        this.Xi = 1800000;
        this.Xj = true;
        this.Xk = false;
        this.connected = true;
        this.Xl = true;
        this.Xm = new au() {
            final /* synthetic */ cy Xq;

            {
                this.Xq = r1;
            }

            public void p(boolean z) {
                this.Xq.a(z, this.Xq.connected);
            }
        };
        this.Xo = false;
    }

    private void bC() {
        this.Xn = new bn(this);
        this.Xn.o(this.Xf);
    }

    private void bD() {
        this.handler = new Handler(this.Xf.getMainLooper(), new Callback() {
            final /* synthetic */ cy Xq;

            {
                this.Xq = r1;
            }

            public boolean handleMessage(Message msg) {
                if (1 == msg.what && cy.ri.equals(msg.obj)) {
                    this.Xq.bp();
                    if (this.Xq.Xi > 0 && !this.Xq.Xo) {
                        this.Xq.handler.sendMessageDelayed(this.Xq.handler.obtainMessage(1, cy.ri), (long) this.Xq.Xi);
                    }
                }
                return true;
            }
        });
        if (this.Xi > 0) {
            this.handler.sendMessageDelayed(this.handler.obtainMessage(1, ri), (long) this.Xi);
        }
    }

    public static cy kh() {
        if (Xp == null) {
            Xp = new cy();
        }
        return Xp;
    }

    synchronized void a(Context context, ar arVar) {
        if (this.Xf == null) {
            this.Xf = context.getApplicationContext();
            if (this.Xh == null) {
                this.Xh = arVar;
            }
        }
    }

    synchronized void a(boolean z, boolean z2) {
        if (!(this.Xo == z && this.connected == z2)) {
            if (z || !z2) {
                if (this.Xi > 0) {
                    this.handler.removeMessages(1, ri);
                }
            }
            if (!z && z2 && this.Xi > 0) {
                this.handler.sendMessageDelayed(this.handler.obtainMessage(1, ri), (long) this.Xi);
            }
            StringBuilder append = new StringBuilder().append("PowerSaveMode ");
            String str = (z || !z2) ? "initiated." : "terminated.";
            bh.v(append.append(str).toString());
            this.Xo = z;
            this.connected = z2;
        }
    }

    synchronized void bF() {
        if (!this.Xo && this.connected && this.Xi > 0) {
            this.handler.removeMessages(1, ri);
            this.handler.sendMessage(this.handler.obtainMessage(1, ri));
        }
    }

    public synchronized void bp() {
        if (this.Xk) {
            this.Xh.a(new Runnable() {
                final /* synthetic */ cy Xq;

                {
                    this.Xq = r1;
                }

                public void run() {
                    this.Xq.Xg.bp();
                }
            });
        } else {
            bh.v("Dispatch call queued. Dispatch will run once initialization is complete.");
            this.Xj = true;
        }
    }

    synchronized at ki() {
        if (this.Xg == null) {
            if (this.Xf == null) {
                throw new IllegalStateException("Cant get a store unless we have a context");
            }
            this.Xg = new cb(this.Xm, this.Xf);
        }
        if (this.handler == null) {
            bD();
        }
        this.Xk = true;
        if (this.Xj) {
            bp();
            this.Xj = false;
        }
        if (this.Xn == null && this.Xl) {
            bC();
        }
        return this.Xg;
    }

    synchronized void q(boolean z) {
        a(this.Xo, z);
    }
}
