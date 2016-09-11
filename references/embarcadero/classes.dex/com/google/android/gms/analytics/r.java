package com.google.android.gms.analytics;

import android.content.Context;
import android.os.Handler;
import android.os.Handler.Callback;
import android.os.Message;
import com.google.android.gms.analytics.u.a;
import com.google.android.gms.location.GeofenceStatusCodes;

class r extends af {
    private static final Object ri;
    private static r ru;
    private Context mContext;
    private Handler mHandler;
    private d rj;
    private volatile f rk;
    private int rl;
    private boolean rm;
    private boolean rn;
    private String ro;
    private boolean rp;
    private boolean rq;
    private e rr;
    private q rs;
    private boolean rt;

    static {
        ri = new Object();
    }

    private r() {
        this.rl = 1800;
        this.rm = true;
        this.rp = true;
        this.rq = true;
        this.rr = new e() {
            final /* synthetic */ r rv;

            {
                this.rv = r1;
            }

            public void p(boolean z) {
                this.rv.a(z, this.rv.rp);
            }
        };
        this.rt = false;
    }

    public static r bB() {
        if (ru == null) {
            ru = new r();
        }
        return ru;
    }

    private void bC() {
        this.rs = new q(this);
        this.rs.o(this.mContext);
    }

    private void bD() {
        this.mHandler = new Handler(this.mContext.getMainLooper(), new Callback() {
            final /* synthetic */ r rv;

            {
                this.rv = r1;
            }

            public boolean handleMessage(Message msg) {
                if (1 == msg.what && r.ri.equals(msg.obj)) {
                    u.bR().r(true);
                    this.rv.dispatchLocalHits();
                    u.bR().r(false);
                    if (this.rv.rl > 0 && !this.rv.rt) {
                        this.rv.mHandler.sendMessageDelayed(this.rv.mHandler.obtainMessage(1, r.ri), (long) (this.rv.rl * GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE));
                    }
                }
                return true;
            }
        });
        if (this.rl > 0) {
            this.mHandler.sendMessageDelayed(this.mHandler.obtainMessage(1, ri), (long) (this.rl * GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE));
        }
    }

    synchronized void a(Context context, f fVar) {
        if (this.mContext == null) {
            this.mContext = context.getApplicationContext();
            if (this.rk == null) {
                this.rk = fVar;
                if (this.rm) {
                    dispatchLocalHits();
                    this.rm = false;
                }
                if (this.rn) {
                    br();
                    this.rn = false;
                }
            }
        }
    }

    synchronized void a(boolean z, boolean z2) {
        if (!(this.rt == z && this.rp == z2)) {
            if (z || !z2) {
                if (this.rl > 0) {
                    this.mHandler.removeMessages(1, ri);
                }
            }
            if (!z && z2 && this.rl > 0) {
                this.mHandler.sendMessageDelayed(this.mHandler.obtainMessage(1, ri), (long) (this.rl * GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE));
            }
            StringBuilder append = new StringBuilder().append("PowerSaveMode ");
            String str = (z || !z2) ? "initiated." : "terminated.";
            aa.v(append.append(str).toString());
            this.rt = z;
            this.rp = z2;
        }
    }

    synchronized d bE() {
        if (this.rj == null) {
            if (this.mContext == null) {
                throw new IllegalStateException("Cant get a store unless we have a context");
            }
            this.rj = new ac(this.rr, this.mContext);
            if (this.ro != null) {
                this.rj.bq().A(this.ro);
                this.ro = null;
            }
        }
        if (this.mHandler == null) {
            bD();
        }
        if (this.rs == null && this.rq) {
            bC();
        }
        return this.rj;
    }

    synchronized void bF() {
        if (!this.rt && this.rp && this.rl > 0) {
            this.mHandler.removeMessages(1, ri);
            this.mHandler.sendMessage(this.mHandler.obtainMessage(1, ri));
        }
    }

    void br() {
        if (this.rk == null) {
            aa.v("setForceLocalDispatch() queued. It will be called once initialization is complete.");
            this.rn = true;
            return;
        }
        u.bR().a(a.SET_FORCE_LOCAL_DISPATCH);
        this.rk.br();
    }

    synchronized void dispatchLocalHits() {
        if (this.rk == null) {
            aa.v("Dispatch call queued. Dispatch will run once initialization is complete.");
            this.rm = true;
        } else {
            u.bR().a(a.DISPATCH);
            this.rk.bp();
        }
    }

    synchronized void q(boolean z) {
        a(this.rt, z);
    }

    synchronized void setLocalDispatchPeriod(int dispatchPeriodInSeconds) {
        if (this.mHandler == null) {
            aa.v("Dispatch period set with null handler. Dispatch will run once initialization is complete.");
            this.rl = dispatchPeriodInSeconds;
        } else {
            u.bR().a(a.SET_DISPATCH_PERIOD);
            if (!this.rt && this.rp && this.rl > 0) {
                this.mHandler.removeMessages(1, ri);
            }
            this.rl = dispatchPeriodInSeconds;
            if (dispatchPeriodInSeconds > 0 && !this.rt && this.rp) {
                this.mHandler.sendMessageDelayed(this.mHandler.obtainMessage(1, ri), (long) (dispatchPeriodInSeconds * GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE));
            }
        }
    }
}
