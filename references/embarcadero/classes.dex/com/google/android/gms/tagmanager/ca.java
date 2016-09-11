package com.google.android.gms.tagmanager;

import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.util.Pair;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.ResultCallback;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.internal.er;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

abstract class ca<R extends Result> implements PendingResult<R> {
    private a<R> VK;
    private final Object zd;
    private final CountDownLatch zf;
    private final ArrayList<com.google.android.gms.common.api.PendingResult.a> zg;
    private ResultCallback<R> zh;
    private volatile R zi;
    private volatile boolean zj;

    public static class a<R extends Result> extends Handler {
        public a() {
            this(Looper.getMainLooper());
        }

        public a(Looper looper) {
            super(looper);
        }

        public void a(ResultCallback<R> resultCallback, R r) {
            sendMessage(obtainMessage(1, new Pair(resultCallback, r)));
        }

        public void a(ca<R> caVar, long j) {
            sendMessageDelayed(obtainMessage(2, caVar), j);
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
                    ca caVar = (ca) msg.obj;
                    caVar.a(caVar.d(Status.zS));
                default:
                    bh.t("Don't know how to handle this message.");
            }
        }
    }

    public ca(Looper looper) {
        this.zd = new Object();
        this.zf = new CountDownLatch(1);
        this.zg = new ArrayList();
        this.VK = new a(looper);
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

    public final void a(R r) {
        synchronized (this.zd) {
            if (isReady()) {
                return;
            }
            er.a(!this.zj, "Result has already been consumed");
            this.zi = r;
            this.zf.countDown();
            Status status = this.zi.getStatus();
            if (this.zh != null) {
                this.VK.dw();
                this.VK.a(this.zh, ds());
            }
            Iterator it = this.zg.iterator();
            while (it.hasNext()) {
                ((com.google.android.gms.common.api.PendingResult.a) it.next()).k(status);
            }
            this.zg.clear();
        }
    }

    public R await() {
        er.a(!this.zj, "Results has already been consumed");
        try {
            this.zf.await();
        } catch (InterruptedException e) {
            a(d(Status.zR));
        }
        er.a(isReady(), "Result is not ready.");
        return ds();
    }

    public R await(long time, TimeUnit units) {
        er.a(!this.zj, "Result has already been consumed.");
        try {
            if (!this.zf.await(time, units)) {
                a(d(Status.zS));
            }
        } catch (InterruptedException e) {
            a(d(Status.zR));
        }
        er.a(isReady(), "Result is not ready.");
        return ds();
    }

    protected abstract R d(Status status);

    void dt() {
        this.zj = true;
        this.zi = null;
    }

    public boolean isReady() {
        return this.zf.getCount() == 0;
    }

    public final void setResultCallback(ResultCallback<R> callback) {
        er.a(!this.zj, "Result has already been consumed.");
        synchronized (this.zd) {
            if (isReady()) {
                this.VK.a((ResultCallback) callback, ds());
            } else {
                this.zh = callback;
            }
        }
    }

    public final void setResultCallback(ResultCallback<R> callback, long time, TimeUnit units) {
        er.a(!this.zj, "Result has already been consumed.");
        synchronized (this.zd) {
            if (isReady()) {
                this.VK.a((ResultCallback) callback, ds());
            } else {
                this.zh = callback;
                this.VK.a(this, units.toMillis(time));
            }
        }
    }
}
