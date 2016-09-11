package com.google.android.gms.analytics;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.IBinder;
import android.os.RemoteException;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.internal.di;
import com.google.android.gms.internal.dj;
import java.util.List;
import java.util.Map;

class c implements b {
    private Context mContext;
    private ServiceConnection qM;
    private b qN;
    private c qO;
    private dj qP;

    final class a implements ServiceConnection {
        final /* synthetic */ c qQ;

        a(c cVar) {
            this.qQ = cVar;
        }

        public void onServiceConnected(ComponentName component, IBinder binder) {
            aa.v("service connected, binder: " + binder);
            try {
                if ("com.google.android.gms.analytics.internal.IAnalyticsService".equals(binder.getInterfaceDescriptor())) {
                    aa.v("bound to service");
                    this.qQ.qP = com.google.android.gms.internal.dj.a.r(binder);
                    this.qQ.bn();
                    return;
                }
            } catch (RemoteException e) {
            }
            this.qQ.mContext.unbindService(this);
            this.qQ.qM = null;
            this.qQ.qO.a(2, null);
        }

        public void onServiceDisconnected(ComponentName component) {
            aa.v("service disconnected: " + component);
            this.qQ.qM = null;
            this.qQ.qN.onDisconnected();
        }
    }

    public interface b {
        void onConnected();

        void onDisconnected();
    }

    public interface c {
        void a(int i, Intent intent);
    }

    public c(Context context, b bVar, c cVar) {
        this.mContext = context;
        if (bVar == null) {
            throw new IllegalArgumentException("onConnectedListener cannot be null");
        }
        this.qN = bVar;
        if (cVar == null) {
            throw new IllegalArgumentException("onConnectionFailedListener cannot be null");
        }
        this.qO = cVar;
    }

    private dj bl() {
        bm();
        return this.qP;
    }

    private void bn() {
        bo();
    }

    private void bo() {
        this.qN.onConnected();
    }

    public void a(Map<String, String> map, long j, String str, List<di> list) {
        try {
            bl().a(map, j, str, list);
        } catch (RemoteException e) {
            aa.t("sendHit failed: " + e);
        }
    }

    public void bk() {
        try {
            bl().bk();
        } catch (RemoteException e) {
            aa.t("clear hits failed: " + e);
        }
    }

    protected void bm() {
        if (!isConnected()) {
            throw new IllegalStateException("Not connected. Call connect() and wait for onConnected() to be called.");
        }
    }

    public void connect() {
        Intent intent = new Intent("com.google.android.gms.analytics.service.START");
        intent.setComponent(new ComponentName(GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_PACKAGE, "com.google.android.gms.analytics.service.AnalyticsService"));
        intent.putExtra(AnalyticsGmsCoreClient.KEY_APP_PACKAGE_NAME, this.mContext.getPackageName());
        if (this.qM != null) {
            aa.t("Calling connect() while still connected, missing disconnect().");
            return;
        }
        this.qM = new a(this);
        boolean bindService = this.mContext.bindService(intent, this.qM, 129);
        aa.v("connect: bindService returned " + bindService + " for " + intent);
        if (!bindService) {
            this.qM = null;
            this.qO.a(1, null);
        }
    }

    public void disconnect() {
        this.qP = null;
        if (this.qM != null) {
            try {
                this.mContext.unbindService(this.qM);
            } catch (IllegalStateException e) {
            } catch (IllegalArgumentException e2) {
            }
            this.qM = null;
            this.qN.onDisconnected();
        }
    }

    public boolean isConnected() {
        return this.qP != null;
    }
}
