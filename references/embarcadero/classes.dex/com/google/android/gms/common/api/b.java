package com.google.android.gms.common.api;

import android.content.Context;
import android.os.Bundle;
import android.os.DeadObjectException;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.util.Log;
import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.api.GoogleApiClient.ApiOptions;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.internal.ee;
import com.google.android.gms.internal.ei;
import com.google.android.gms.internal.er;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

final class b implements GoogleApiClient {
    private int zA;
    private int zB;
    private int zC;
    private boolean zD;
    private int zE;
    private long zF;
    final Handler zG;
    private final Bundle zH;
    private final Map<com.google.android.gms.common.api.Api.b<?>, com.google.android.gms.common.api.Api.a> zI;
    private boolean zJ;
    final Set<c> zK;
    final ConnectionCallbacks zL;
    private final com.google.android.gms.internal.ei.b zM;
    private final a zm;
    private final Lock zv;
    private final Condition zw;
    private final ei zx;
    final Queue<c<?>> zy;
    private ConnectionResult zz;

    interface a {
        void b(c cVar);
    }

    class b extends Handler {
        final /* synthetic */ b zN;

        b(b bVar, Looper looper) {
            this.zN = bVar;
            super(looper);
        }

        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                this.zN.zv.lock();
                try {
                    if (!(this.zN.isConnected() || this.zN.isConnecting())) {
                        this.zN.connect();
                    }
                    this.zN.zv.unlock();
                } catch (Throwable th) {
                    this.zN.zv.unlock();
                }
            } else {
                Log.wtf("GoogleApiClientImpl", "Don't know how to handle this message.");
            }
        }
    }

    interface c<A extends com.google.android.gms.common.api.Api.a> {
        void a(a aVar);

        void b(A a) throws DeadObjectException;

        com.google.android.gms.common.api.Api.b<A> dp();

        int dr();

        void du();
    }

    /* renamed from: com.google.android.gms.common.api.b.4 */
    class AnonymousClass4 implements OnConnectionFailedListener {
        final /* synthetic */ b zN;
        final /* synthetic */ com.google.android.gms.common.api.Api.b zO;

        AnonymousClass4(b bVar, com.google.android.gms.common.api.Api.b bVar2) {
            this.zN = bVar;
            this.zO = bVar2;
        }

        public void onConnectionFailed(ConnectionResult result) {
            this.zN.zv.lock();
            try {
                if (this.zN.zz == null || this.zO.getPriority() < this.zN.zA) {
                    this.zN.zz = result;
                    this.zN.zA = this.zO.getPriority();
                }
                this.zN.dy();
            } finally {
                this.zN.zv.unlock();
            }
        }
    }

    public b(Context context, Looper looper, ee eeVar, Map<Api, ApiOptions> map, Set<ConnectionCallbacks> set, Set<OnConnectionFailedListener> set2) {
        this.zv = new ReentrantLock();
        this.zw = this.zv.newCondition();
        this.zy = new LinkedList();
        this.zB = 4;
        this.zC = 0;
        this.zD = false;
        this.zF = 5000;
        this.zH = new Bundle();
        this.zI = new HashMap();
        this.zK = new HashSet();
        this.zm = new a() {
            final /* synthetic */ b zN;

            {
                this.zN = r1;
            }

            public void b(c cVar) {
                this.zN.zv.lock();
                try {
                    this.zN.zK.remove(cVar);
                } finally {
                    this.zN.zv.unlock();
                }
            }
        };
        this.zL = new ConnectionCallbacks() {
            final /* synthetic */ b zN;

            {
                this.zN = r1;
            }

            public void onConnected(Bundle connectionHint) {
                this.zN.zv.lock();
                try {
                    if (this.zN.zB == 1) {
                        if (connectionHint != null) {
                            this.zN.zH.putAll(connectionHint);
                        }
                        this.zN.dy();
                    }
                    this.zN.zv.unlock();
                } catch (Throwable th) {
                    this.zN.zv.unlock();
                }
            }

            public void onConnectionSuspended(int cause) {
                this.zN.zv.lock();
                try {
                    this.zN.G(cause);
                    switch (cause) {
                        case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                            if (!this.zN.dA()) {
                                this.zN.zC = 2;
                                this.zN.zG.sendMessageDelayed(this.zN.zG.obtainMessage(1), this.zN.zF);
                                break;
                            }
                            this.zN.zv.unlock();
                            return;
                        case DetectedActivity.ON_FOOT /*2*/:
                            this.zN.connect();
                            break;
                    }
                    this.zN.zv.unlock();
                } catch (Throwable th) {
                    this.zN.zv.unlock();
                }
            }
        };
        this.zM = new com.google.android.gms.internal.ei.b() {
            final /* synthetic */ b zN;

            {
                this.zN = r1;
            }

            public Bundle cY() {
                return null;
            }

            public boolean dC() {
                return this.zN.zJ;
            }

            public boolean isConnected() {
                return this.zN.isConnected();
            }
        };
        this.zx = new ei(context, looper, this.zM);
        this.zG = new b(this, looper);
        for (ConnectionCallbacks registerConnectionCallbacks : set) {
            this.zx.registerConnectionCallbacks(registerConnectionCallbacks);
        }
        for (OnConnectionFailedListener registerConnectionFailedListener : set2) {
            this.zx.registerConnectionFailedListener(registerConnectionFailedListener);
        }
        for (Api api : map.keySet()) {
            com.google.android.gms.common.api.Api.b dp = api.dp();
            ApiOptions apiOptions = (ApiOptions) map.get(api);
            this.zI.put(dp, dp.b(context, looper, eeVar, apiOptions, this.zL, new AnonymousClass4(this, dp)));
        }
    }

    private void G(int i) {
        this.zv.lock();
        try {
            if (this.zB != 3) {
                if (i == -1) {
                    if (isConnecting()) {
                        Iterator it = this.zy.iterator();
                        while (it.hasNext()) {
                            if (((c) it.next()).dr() != 1) {
                                it.remove();
                            }
                        }
                    } else {
                        this.zy.clear();
                    }
                    if (this.zz == null && !this.zy.isEmpty()) {
                        this.zD = true;
                        return;
                    }
                }
                boolean isConnecting = isConnecting();
                boolean isConnected = isConnected();
                this.zB = 3;
                if (isConnecting) {
                    if (i == -1) {
                        this.zz = null;
                    }
                    this.zw.signalAll();
                }
                for (c du : this.zK) {
                    du.du();
                }
                this.zK.clear();
                this.zJ = false;
                for (com.google.android.gms.common.api.Api.a aVar : this.zI.values()) {
                    if (aVar.isConnected()) {
                        aVar.disconnect();
                    }
                }
                this.zJ = true;
                this.zB = 4;
                if (isConnected) {
                    if (i != -1) {
                        this.zx.P(i);
                    }
                    this.zJ = false;
                }
            }
            this.zv.unlock();
        } finally {
            this.zv.unlock();
        }
    }

    private <A extends com.google.android.gms.common.api.Api.a> void a(c<A> cVar) throws DeadObjectException {
        this.zv.lock();
        try {
            er.a(isConnected(), "GoogleApiClient is not connected yet.");
            er.a(cVar.dp() != null, "This task can not be executed or enqueued (it's probably a Batch or malformed)");
            if (cVar instanceof Releasable) {
                this.zK.add(cVar);
                cVar.a(this.zm);
            }
            cVar.b(a(cVar.dp()));
        } finally {
            this.zv.unlock();
        }
    }

    private boolean dA() {
        this.zv.lock();
        try {
            boolean z = this.zC != 0;
            this.zv.unlock();
            return z;
        } catch (Throwable th) {
            this.zv.unlock();
        }
    }

    private void dB() {
        this.zv.lock();
        try {
            this.zC = 0;
            this.zG.removeMessages(1);
        } finally {
            this.zv.unlock();
        }
    }

    private void dy() {
        this.zv.lock();
        try {
            this.zE--;
            if (this.zE == 0) {
                if (this.zz != null) {
                    this.zD = false;
                    G(3);
                    if (dA()) {
                        this.zC--;
                    }
                    if (dA()) {
                        this.zG.sendMessageDelayed(this.zG.obtainMessage(1), this.zF);
                    } else {
                        this.zx.a(this.zz);
                    }
                    this.zJ = false;
                } else {
                    this.zB = 2;
                    dB();
                    this.zw.signalAll();
                    dz();
                    if (this.zD) {
                        this.zD = false;
                        G(-1);
                    } else {
                        this.zx.b(this.zH.isEmpty() ? null : this.zH);
                    }
                }
            }
            this.zv.unlock();
        } catch (Throwable th) {
            this.zv.unlock();
        }
    }

    private void dz() {
        er.a(isConnected(), "GoogleApiClient is not connected yet.");
        this.zv.lock();
        while (!this.zy.isEmpty()) {
            try {
                a((c) this.zy.remove());
            } catch (Throwable e) {
                Log.w("GoogleApiClientImpl", "Service died while flushing queue", e);
            } catch (Throwable th) {
                this.zv.unlock();
            }
        }
        this.zv.unlock();
    }

    public <C extends com.google.android.gms.common.api.Api.a> C a(com.google.android.gms.common.api.Api.b<C> bVar) {
        Object obj = (com.google.android.gms.common.api.Api.a) this.zI.get(bVar);
        er.b(obj, (Object) "Appropriate Api was not requested.");
        return obj;
    }

    public <A extends com.google.android.gms.common.api.Api.a, T extends com.google.android.gms.common.api.a.a<? extends Result, A>> T a(T t) {
        this.zv.lock();
        try {
            if (isConnected()) {
                b((com.google.android.gms.common.api.a.a) t);
            } else {
                this.zy.add(t);
            }
            this.zv.unlock();
            return t;
        } catch (Throwable th) {
            this.zv.unlock();
        }
    }

    public <A extends com.google.android.gms.common.api.Api.a, T extends com.google.android.gms.common.api.a.a<? extends Result, A>> T b(T t) {
        er.a(isConnected(), "GoogleApiClient is not connected yet.");
        dz();
        try {
            a((c) t);
        } catch (DeadObjectException e) {
            G(1);
        }
        return t;
    }

    public ConnectionResult blockingConnect(long timeout, TimeUnit unit) {
        ConnectionResult connectionResult;
        er.a(Looper.myLooper() != Looper.getMainLooper(), "blockingConnect must not be called on the UI thread");
        this.zv.lock();
        try {
            connect();
            long toNanos = unit.toNanos(timeout);
            while (isConnecting()) {
                toNanos = this.zw.awaitNanos(toNanos);
                if (toNanos <= 0) {
                    connectionResult = new ConnectionResult(14, null);
                    break;
                }
            }
            if (isConnected()) {
                connectionResult = ConnectionResult.yI;
                this.zv.unlock();
            } else if (this.zz != null) {
                connectionResult = this.zz;
                this.zv.unlock();
            } else {
                connectionResult = new ConnectionResult(13, null);
                this.zv.unlock();
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            connectionResult = new ConnectionResult(15, null);
        } finally {
            this.zv.unlock();
        }
        return connectionResult;
    }

    public void connect() {
        this.zv.lock();
        try {
            this.zD = false;
            if (isConnected() || isConnecting()) {
                this.zv.unlock();
                return;
            }
            this.zJ = true;
            this.zz = null;
            this.zB = 1;
            this.zH.clear();
            this.zE = this.zI.size();
            for (com.google.android.gms.common.api.Api.a connect : this.zI.values()) {
                connect.connect();
            }
            this.zv.unlock();
        } catch (Throwable th) {
            this.zv.unlock();
        }
    }

    public void disconnect() {
        dB();
        G(-1);
    }

    public boolean isConnected() {
        this.zv.lock();
        try {
            boolean z = this.zB == 2;
            this.zv.unlock();
            return z;
        } catch (Throwable th) {
            this.zv.unlock();
        }
    }

    public boolean isConnecting() {
        boolean z = true;
        this.zv.lock();
        try {
            if (this.zB != 1) {
                z = false;
            }
            this.zv.unlock();
            return z;
        } catch (Throwable th) {
            this.zv.unlock();
        }
    }

    public boolean isConnectionCallbacksRegistered(ConnectionCallbacks listener) {
        return this.zx.isConnectionCallbacksRegistered(listener);
    }

    public boolean isConnectionFailedListenerRegistered(OnConnectionFailedListener listener) {
        return this.zx.isConnectionFailedListenerRegistered(listener);
    }

    public void reconnect() {
        disconnect();
        connect();
    }

    public void registerConnectionCallbacks(ConnectionCallbacks listener) {
        this.zx.registerConnectionCallbacks(listener);
    }

    public void registerConnectionFailedListener(OnConnectionFailedListener listener) {
        this.zx.registerConnectionFailedListener(listener);
    }

    public void unregisterConnectionCallbacks(ConnectionCallbacks listener) {
        this.zx.unregisterConnectionCallbacks(listener);
    }

    public void unregisterConnectionFailedListener(OnConnectionFailedListener listener) {
        this.zx.unregisterConnectionFailedListener(listener);
    }
}
