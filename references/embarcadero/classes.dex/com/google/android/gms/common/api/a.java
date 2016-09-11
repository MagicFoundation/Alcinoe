package com.google.android.gms.common.api;

import android.os.DeadObjectException;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.os.RemoteException;
import android.util.Log;
import android.util.Pair;
import com.google.android.gms.internal.er;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class a {

    public static class b<R extends Result> extends Handler {
        public b() {
            this(Looper.getMainLooper());
        }

        public b(Looper looper) {
            super(looper);
        }

        public void a(ResultCallback<R> resultCallback, R r) {
            sendMessage(obtainMessage(1, new Pair(resultCallback, r)));
        }

        public void a(a<R, ?> aVar, long j) {
            sendMessageDelayed(obtainMessage(2, aVar), j);
        }

        protected void b(ResultCallback<R> resultCallback, R r) {
            resultCallback.onResult(r);
        }

        public void dw() {
            removeMessages(2);
        }

        public void handleMessage(Message msg) {
            switch (msg.what) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    Pair pair = (Pair) msg.obj;
                    b((ResultCallback) pair.first, (Result) pair.second);
                case DetectedActivity.ON_FOOT /*2*/:
                    a aVar = (a) msg.obj;
                    aVar.a(aVar.d(Status.zS));
                default:
                    Log.wtf("GoogleApi", "Don't know how to handle this message.");
            }
        }
    }

    public interface c<R> {
        void b(R r);
    }

    public static abstract class a<R extends Result, A extends com.google.android.gms.common.api.Api.a> implements PendingResult<R>, c<R>, c<A> {
        private final com.google.android.gms.common.api.Api.b<A> zc;
        private final Object zd;
        private b<R> ze;
        private final CountDownLatch zf;
        private final ArrayList<com.google.android.gms.common.api.PendingResult.a> zg;
        private ResultCallback<R> zh;
        private volatile R zi;
        private volatile boolean zj;
        private boolean zk;
        private boolean zl;
        private a zm;

        protected a() {
            this((com.google.android.gms.common.api.Api.b) null);
        }

        protected a(com.google.android.gms.common.api.Api.b<A> bVar) {
            this.zd = new Object();
            this.zf = new CountDownLatch(1);
            this.zg = new ArrayList();
            this.zc = bVar;
        }

        private void a(RemoteException remoteException) {
            a(d(new Status(8, remoteException.getLocalizedMessage(), null)));
        }

        private R ds() {
            R r;
            synchronized (this.zd) {
                er.a(!this.zj, "Result has already been consumed.");
                er.a(isReady(), "Result is not ready.");
                r = this.zi;
                dt();
            }
            return r;
        }

        private void dv() {
            if (this.zi != null && (this instanceof Releasable)) {
                try {
                    ((Releasable) this).release();
                } catch (Throwable e) {
                    Log.w("GoogleApi", "Unable to release " + this, e);
                }
            }
        }

        protected abstract void a(A a) throws RemoteException;

        public final void a(R r) {
            boolean z = true;
            synchronized (this.zd) {
                if (this.zl) {
                    if (r instanceof Releasable) {
                        ((Releasable) r).release();
                    }
                    return;
                }
                er.a(!isReady(), "Results have already been set");
                if (this.zj) {
                    z = false;
                }
                er.a(z, "Result has already been consumed");
                this.zi = r;
                if (this.zk) {
                    dv();
                    return;
                }
                this.zf.countDown();
                Status status = this.zi.getStatus();
                if (this.zh != null) {
                    this.ze.dw();
                    this.ze.a(this.zh, ds());
                }
                Iterator it = this.zg.iterator();
                while (it.hasNext()) {
                    ((com.google.android.gms.common.api.PendingResult.a) it.next()).k(status);
                }
                this.zg.clear();
            }
        }

        public void a(a aVar) {
            this.zm = aVar;
        }

        public final R await() {
            boolean z = false;
            er.a(!this.zj, "Results has already been consumed");
            if (isReady() || Looper.myLooper() != Looper.getMainLooper()) {
                z = true;
            }
            er.a(z, "await must not be called on the UI thread");
            try {
                this.zf.await();
            } catch (InterruptedException e) {
                synchronized (this.zd) {
                }
                a(d(Status.zR));
                this.zl = true;
            }
            er.a(isReady(), "Result is not ready.");
            return ds();
        }

        public final R await(long time, TimeUnit units) {
            boolean z = false;
            er.a(!this.zj, "Result has already been consumed.");
            if (isReady() || Looper.myLooper() != Looper.getMainLooper()) {
                z = true;
            }
            er.a(z, "await must not be called on the UI thread");
            try {
                if (!this.zf.await(time, units)) {
                    synchronized (this.zd) {
                        a(d(Status.zS));
                        this.zl = true;
                    }
                }
            } catch (InterruptedException e) {
                synchronized (this.zd) {
                }
                a(d(Status.zR));
                this.zl = true;
            }
            er.a(isReady(), "Result is not ready.");
            return ds();
        }

        public final void b(A a) throws DeadObjectException {
            this.ze = new b(a.getLooper());
            try {
                a((com.google.android.gms.common.api.Api.a) a);
            } catch (RemoteException e) {
                a(e);
                throw e;
            } catch (RemoteException e2) {
                a(e2);
            }
        }

        public /* synthetic */ void b(Object obj) {
            a((Result) obj);
        }

        protected abstract R d(Status status);

        public final com.google.android.gms.common.api.Api.b<A> dp() {
            return this.zc;
        }

        public int dr() {
            return 0;
        }

        void dt() {
            this.zj = true;
            this.zi = null;
            if (this.zm != null) {
                this.zm.b(this);
            }
        }

        public void du() {
            dv();
            this.zk = true;
        }

        public final boolean isReady() {
            return this.zf.getCount() == 0;
        }

        public final void setResultCallback(ResultCallback<R> callback) {
            er.a(!this.zj, "Result has already been consumed.");
            synchronized (this.zd) {
                if (isReady()) {
                    this.ze.a((ResultCallback) callback, ds());
                } else {
                    this.zh = callback;
                }
            }
        }

        public final void setResultCallback(ResultCallback<R> callback, long time, TimeUnit units) {
            er.a(!this.zj, "Result has already been consumed.");
            synchronized (this.zd) {
                if (isReady()) {
                    this.ze.a((ResultCallback) callback, ds());
                } else {
                    this.zh = callback;
                    this.ze.a(this, units.toMillis(time));
                }
            }
        }
    }
}
