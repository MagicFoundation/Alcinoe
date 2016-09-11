package com.google.android.gms.internal;

import android.app.PendingIntent;
import android.content.ComponentName;
import android.content.Context;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Looper;
import android.os.Message;
import android.os.RemoteException;
import android.util.Log;
import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.GooglePlayServicesClient;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;

public abstract class eh<T extends IInterface> implements GooglePlayServicesClient, com.google.android.gms.common.api.Api.a, com.google.android.gms.internal.ei.b {
    public static final String[] BB;
    boolean BA;
    private T Bv;
    private final ArrayList<b<?>> Bw;
    private f Bx;
    private volatile int By;
    private final String[] Bz;
    private final Context mContext;
    final Handler mHandler;
    private final Looper zs;
    private final ei zx;

    final class a extends Handler {
        final /* synthetic */ eh BC;

        public a(eh ehVar, Looper looper) {
            this.BC = ehVar;
            super(looper);
        }

        public void handleMessage(Message msg) {
            b bVar;
            if (msg.what == 1 && !this.BC.isConnecting()) {
                bVar = (b) msg.obj;
                bVar.cP();
                bVar.unregister();
            } else if (msg.what == 3) {
                this.BC.zx.a(new ConnectionResult(((Integer) msg.obj).intValue(), null));
            } else if (msg.what == 4) {
                this.BC.By = 1;
                this.BC.Bv = null;
                this.BC.zx.P(((Integer) msg.obj).intValue());
            } else if (msg.what == 2 && !this.BC.isConnected()) {
                bVar = (b) msg.obj;
                bVar.cP();
                bVar.unregister();
            } else if (msg.what == 2 || msg.what == 1) {
                ((b) msg.obj).ec();
            } else {
                Log.wtf("GmsClient", "Don't know how to handle this message.");
            }
        }
    }

    protected abstract class b<TListener> {
        final /* synthetic */ eh BC;
        private boolean BD;
        private TListener mListener;

        public b(eh ehVar, TListener tListener) {
            this.BC = ehVar;
            this.mListener = tListener;
            this.BD = false;
        }

        protected abstract void a(TListener tListener);

        protected abstract void cP();

        public void ec() {
            synchronized (this) {
                Object obj = this.mListener;
                if (this.BD) {
                    Log.w("GmsClient", "Callback proxy " + this + " being reused. This is not safe.");
                }
            }
            if (obj != null) {
                try {
                    a(obj);
                } catch (RuntimeException e) {
                    cP();
                    throw e;
                }
            }
            cP();
            synchronized (this) {
                this.BD = true;
            }
            unregister();
        }

        public void ed() {
            synchronized (this) {
                this.mListener = null;
            }
        }

        public void unregister() {
            ed();
            synchronized (this.BC.Bw) {
                this.BC.Bw.remove(this);
            }
        }
    }

    final class f implements ServiceConnection {
        final /* synthetic */ eh BC;

        f(eh ehVar) {
            this.BC = ehVar;
        }

        public void onServiceConnected(ComponentName component, IBinder binder) {
            this.BC.x(binder);
        }

        public void onServiceDisconnected(ComponentName component) {
            this.BC.mHandler.sendMessage(this.BC.mHandler.obtainMessage(4, Integer.valueOf(1)));
        }
    }

    public static final class c implements ConnectionCallbacks {
        private final GooglePlayServicesClient.ConnectionCallbacks BE;

        public c(GooglePlayServicesClient.ConnectionCallbacks connectionCallbacks) {
            this.BE = connectionCallbacks;
        }

        public boolean equals(Object other) {
            return other instanceof c ? this.BE.equals(((c) other).BE) : this.BE.equals(other);
        }

        public void onConnected(Bundle connectionHint) {
            this.BE.onConnected(connectionHint);
        }

        public void onConnectionSuspended(int cause) {
            this.BE.onDisconnected();
        }
    }

    public abstract class d<TListener> extends b<TListener> {
        final /* synthetic */ eh BC;
        private final DataHolder zU;

        public d(eh ehVar, TListener tListener, DataHolder dataHolder) {
            this.BC = ehVar;
            super(ehVar, tListener);
            this.zU = dataHolder;
        }

        protected final void a(TListener tListener) {
            a(tListener, this.zU);
        }

        protected abstract void a(TListener tListener, DataHolder dataHolder);

        protected void cP() {
            if (this.zU != null) {
                this.zU.close();
            }
        }

        public /* bridge */ /* synthetic */ void ec() {
            super.ec();
        }

        public /* bridge */ /* synthetic */ void ed() {
            super.ed();
        }

        public /* bridge */ /* synthetic */ void unregister() {
            super.unregister();
        }
    }

    protected final class h extends b<Boolean> {
        final /* synthetic */ eh BC;
        public final Bundle BH;
        public final IBinder BI;
        public final int statusCode;

        public h(eh ehVar, int i, IBinder iBinder, Bundle bundle) {
            this.BC = ehVar;
            super(ehVar, Boolean.valueOf(true));
            this.statusCode = i;
            this.BI = iBinder;
            this.BH = bundle;
        }

        protected /* synthetic */ void a(Object obj) {
            b((Boolean) obj);
        }

        protected void b(Boolean bool) {
            if (bool == null) {
                this.BC.By = 1;
                return;
            }
            switch (this.statusCode) {
                case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                    try {
                        if (this.BC.aG().equals(this.BI.getInterfaceDescriptor())) {
                            this.BC.Bv = this.BC.p(this.BI);
                            if (this.BC.Bv != null) {
                                this.BC.By = 3;
                                this.BC.zx.bo();
                                return;
                            }
                        }
                    } catch (RemoteException e) {
                    }
                    ej.y(this.BC.mContext).b(this.BC.aF(), this.BC.Bx);
                    this.BC.Bx = null;
                    this.BC.By = 1;
                    this.BC.Bv = null;
                    this.BC.zx.a(new ConnectionResult(8, null));
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    this.BC.By = 1;
                    throw new IllegalStateException("A fatal developer error has occurred. Check the logs for further information.");
                default:
                    PendingIntent pendingIntent = this.BH != null ? (PendingIntent) this.BH.getParcelable("pendingIntent") : null;
                    if (this.BC.Bx != null) {
                        ej.y(this.BC.mContext).b(this.BC.aF(), this.BC.Bx);
                        this.BC.Bx = null;
                    }
                    this.BC.By = 1;
                    this.BC.Bv = null;
                    this.BC.zx.a(new ConnectionResult(this.statusCode, pendingIntent));
            }
        }

        protected void cP() {
        }
    }

    public static final class e extends com.google.android.gms.internal.em.a {
        private eh BF;

        public e(eh ehVar) {
            this.BF = ehVar;
        }

        public void b(int i, IBinder iBinder, Bundle bundle) {
            er.b((Object) "onPostInitComplete can be called only once per call to getServiceFromBroker", this.BF);
            this.BF.a(i, iBinder, bundle);
            this.BF = null;
        }
    }

    public static final class g implements OnConnectionFailedListener {
        private final GooglePlayServicesClient.OnConnectionFailedListener BG;

        public g(GooglePlayServicesClient.OnConnectionFailedListener onConnectionFailedListener) {
            this.BG = onConnectionFailedListener;
        }

        public boolean equals(Object other) {
            return other instanceof g ? this.BG.equals(((g) other).BG) : this.BG.equals(other);
        }

        public void onConnectionFailed(ConnectionResult result) {
            this.BG.onConnectionFailed(result);
        }
    }

    static {
        BB = new String[]{"service_esmobile", "service_googleme"};
    }

    protected eh(Context context, Looper looper, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener, String... strArr) {
        this.Bw = new ArrayList();
        this.By = 1;
        this.BA = false;
        this.mContext = (Context) er.f(context);
        this.zs = (Looper) er.b((Object) looper, (Object) "Looper must not be null");
        this.zx = new ei(context, looper, this);
        this.mHandler = new a(this, looper);
        b(strArr);
        this.Bz = strArr;
        registerConnectionCallbacks((ConnectionCallbacks) er.f(connectionCallbacks));
        registerConnectionFailedListener((OnConnectionFailedListener) er.f(onConnectionFailedListener));
    }

    protected eh(Context context, GooglePlayServicesClient.ConnectionCallbacks connectionCallbacks, GooglePlayServicesClient.OnConnectionFailedListener onConnectionFailedListener, String... strArr) {
        this(context, context.getMainLooper(), new c(connectionCallbacks), new g(onConnectionFailedListener), strArr);
    }

    public void O(int i) {
        this.mHandler.sendMessage(this.mHandler.obtainMessage(4, Integer.valueOf(i)));
    }

    protected void a(int i, IBinder iBinder, Bundle bundle) {
        this.mHandler.sendMessage(this.mHandler.obtainMessage(1, new h(this, i, iBinder, bundle)));
    }

    public final void a(b<?> bVar) {
        synchronized (this.Bw) {
            this.Bw.add(bVar);
        }
        this.mHandler.sendMessage(this.mHandler.obtainMessage(2, bVar));
    }

    protected abstract void a(en enVar, e eVar) throws RemoteException;

    protected abstract String aF();

    protected abstract String aG();

    protected void b(String... strArr) {
    }

    protected final void bm() {
        if (!isConnected()) {
            throw new IllegalStateException("Not connected. Call connect() and wait for onConnected() to be called.");
        }
    }

    public Bundle cY() {
        return null;
    }

    public void connect() {
        this.BA = true;
        this.By = 2;
        int isGooglePlayServicesAvailable = GooglePlayServicesUtil.isGooglePlayServicesAvailable(this.mContext);
        if (isGooglePlayServicesAvailable != 0) {
            this.By = 1;
            this.mHandler.sendMessage(this.mHandler.obtainMessage(3, Integer.valueOf(isGooglePlayServicesAvailable)));
            return;
        }
        if (this.Bx != null) {
            Log.e("GmsClient", "Calling connect() while still connected, missing disconnect().");
            this.Bv = null;
            ej.y(this.mContext).b(aF(), this.Bx);
        }
        this.Bx = new f(this);
        if (!ej.y(this.mContext).a(aF(), this.Bx)) {
            Log.e("GmsClient", "unable to connect to service: " + aF());
            this.mHandler.sendMessage(this.mHandler.obtainMessage(3, Integer.valueOf(9)));
        }
    }

    public boolean dC() {
        return this.BA;
    }

    public void disconnect() {
        this.BA = false;
        synchronized (this.Bw) {
            int size = this.Bw.size();
            for (int i = 0; i < size; i++) {
                ((b) this.Bw.get(i)).ed();
            }
            this.Bw.clear();
        }
        this.By = 1;
        this.Bv = null;
        if (this.Bx != null) {
            ej.y(this.mContext).b(aF(), this.Bx);
            this.Bx = null;
        }
    }

    public final String[] ea() {
        return this.Bz;
    }

    protected final T eb() {
        bm();
        return this.Bv;
    }

    public final Context getContext() {
        return this.mContext;
    }

    public final Looper getLooper() {
        return this.zs;
    }

    public boolean isConnected() {
        return this.By == 3;
    }

    public boolean isConnecting() {
        return this.By == 2;
    }

    public boolean isConnectionCallbacksRegistered(GooglePlayServicesClient.ConnectionCallbacks listener) {
        return this.zx.isConnectionCallbacksRegistered(new c(listener));
    }

    public boolean isConnectionFailedListenerRegistered(GooglePlayServicesClient.OnConnectionFailedListener listener) {
        return this.zx.isConnectionFailedListenerRegistered(listener);
    }

    protected abstract T p(IBinder iBinder);

    public void registerConnectionCallbacks(GooglePlayServicesClient.ConnectionCallbacks listener) {
        this.zx.registerConnectionCallbacks(new c(listener));
    }

    public void registerConnectionCallbacks(ConnectionCallbacks listener) {
        this.zx.registerConnectionCallbacks(listener);
    }

    public void registerConnectionFailedListener(GooglePlayServicesClient.OnConnectionFailedListener listener) {
        this.zx.registerConnectionFailedListener(listener);
    }

    public void registerConnectionFailedListener(OnConnectionFailedListener listener) {
        this.zx.registerConnectionFailedListener(listener);
    }

    public void unregisterConnectionCallbacks(GooglePlayServicesClient.ConnectionCallbacks listener) {
        this.zx.unregisterConnectionCallbacks(new c(listener));
    }

    public void unregisterConnectionFailedListener(GooglePlayServicesClient.OnConnectionFailedListener listener) {
        this.zx.unregisterConnectionFailedListener(listener);
    }

    protected final void x(IBinder iBinder) {
        try {
            a(com.google.android.gms.internal.en.a.z(iBinder), new e(this));
        } catch (RemoteException e) {
            Log.w("GmsClient", "service died");
        }
    }
}
