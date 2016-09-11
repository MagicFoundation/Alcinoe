package com.google.analytics.tracking.android;

import android.content.Context;
import android.content.IntentFilter;
import android.os.Handler;
import android.os.Handler.Callback;
import android.os.Message;
import com.google.analytics.tracking.android.GAUsage.Field;
import com.google.android.gms.common.util.VisibleForTesting;
import com.google.android.gms.location.GeofenceStatusCodes;

public class GAServiceManager implements ServiceManager {
    private static final int MSG_KEY = 1;
    private static final Object MSG_OBJECT;
    private static GAServiceManager instance;
    private boolean connected;
    private Context ctx;
    private int dispatchPeriodInSeconds;
    private Handler handler;
    private boolean listenForNetwork;
    private AnalyticsStoreStateListener listener;
    private GANetworkReceiver networkReceiver;
    private boolean pendingDispatch;
    private AnalyticsStore store;
    private boolean storeIsEmpty;
    private volatile AnalyticsThread thread;

    static {
        MSG_OBJECT = new Object();
    }

    public static GAServiceManager getInstance() {
        if (instance == null) {
            instance = new GAServiceManager();
        }
        return instance;
    }

    private GAServiceManager() {
        this.dispatchPeriodInSeconds = 1800;
        this.pendingDispatch = true;
        this.connected = true;
        this.listenForNetwork = true;
        this.listener = new AnalyticsStoreStateListener() {
            public void reportStoreIsEmpty(boolean isEmpty) {
                GAServiceManager.this.updatePowerSaveMode(isEmpty, GAServiceManager.this.connected);
            }
        };
        this.storeIsEmpty = false;
    }

    @VisibleForTesting
    GAServiceManager(Context ctx, AnalyticsThread thread, AnalyticsStore store, boolean listenForNetwork) {
        this.dispatchPeriodInSeconds = 1800;
        this.pendingDispatch = true;
        this.connected = true;
        this.listenForNetwork = true;
        this.listener = new AnalyticsStoreStateListener() {
            public void reportStoreIsEmpty(boolean isEmpty) {
                GAServiceManager.this.updatePowerSaveMode(isEmpty, GAServiceManager.this.connected);
            }
        };
        this.storeIsEmpty = false;
        this.store = store;
        this.thread = thread;
        this.listenForNetwork = listenForNetwork;
        initialize(ctx, thread);
    }

    private void initializeNetworkReceiver() {
        this.networkReceiver = new GANetworkReceiver(this);
        IntentFilter filter = new IntentFilter();
        filter.addAction("android.net.conn.CONNECTIVITY_CHANGE");
        this.ctx.registerReceiver(this.networkReceiver, filter);
    }

    private void initializeHandler() {
        this.handler = new Handler(this.ctx.getMainLooper(), new Callback() {
            public boolean handleMessage(Message msg) {
                if (GAServiceManager.MSG_KEY == msg.what && GAServiceManager.MSG_OBJECT.equals(msg.obj)) {
                    GAUsage.getInstance().setDisableUsage(true);
                    GAServiceManager.this.dispatch();
                    GAUsage.getInstance().setDisableUsage(false);
                    if (GAServiceManager.this.dispatchPeriodInSeconds > 0 && !GAServiceManager.this.storeIsEmpty) {
                        GAServiceManager.this.handler.sendMessageDelayed(GAServiceManager.this.handler.obtainMessage(GAServiceManager.MSG_KEY, GAServiceManager.MSG_OBJECT), (long) (GAServiceManager.this.dispatchPeriodInSeconds * GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE));
                    }
                }
                return true;
            }
        });
        if (this.dispatchPeriodInSeconds > 0) {
            this.handler.sendMessageDelayed(this.handler.obtainMessage(MSG_KEY, MSG_OBJECT), (long) (this.dispatchPeriodInSeconds * GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE));
        }
    }

    synchronized void initialize(Context ctx, AnalyticsThread thread) {
        if (this.ctx == null) {
            this.ctx = ctx.getApplicationContext();
            if (this.thread == null) {
                this.thread = thread;
                if (this.pendingDispatch) {
                    thread.dispatch();
                }
            }
        }
    }

    @VisibleForTesting
    AnalyticsStoreStateListener getListener() {
        return this.listener;
    }

    synchronized AnalyticsStore getStore() {
        if (this.store == null) {
            if (this.ctx == null) {
                throw new IllegalStateException("Cant get a store unless we have a context");
            }
            this.store = new PersistentAnalyticsStore(this.listener, this.ctx);
        }
        if (this.handler == null) {
            initializeHandler();
        }
        if (this.networkReceiver == null && this.listenForNetwork) {
            initializeNetworkReceiver();
        }
        return this.store;
    }

    public synchronized void dispatch() {
        if (this.thread == null) {
            Log.w("dispatch call queued.  Need to call GAServiceManager.getInstance().initialize().");
            this.pendingDispatch = true;
        } else {
            GAUsage.getInstance().setUsage(Field.DISPATCH);
            this.thread.dispatch();
        }
    }

    public synchronized void setDispatchPeriod(int dispatchPeriodInSeconds) {
        if (this.handler == null) {
            Log.w("Need to call initialize() and be in fallback mode to start dispatch.");
            this.dispatchPeriodInSeconds = dispatchPeriodInSeconds;
        } else {
            GAUsage.getInstance().setUsage(Field.SET_DISPATCH_PERIOD);
            if (!this.storeIsEmpty && this.connected && this.dispatchPeriodInSeconds > 0) {
                this.handler.removeMessages(MSG_KEY, MSG_OBJECT);
            }
            this.dispatchPeriodInSeconds = dispatchPeriodInSeconds;
            if (dispatchPeriodInSeconds > 0 && !this.storeIsEmpty && this.connected) {
                this.handler.sendMessageDelayed(this.handler.obtainMessage(MSG_KEY, MSG_OBJECT), (long) (dispatchPeriodInSeconds * GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE));
            }
        }
    }

    @VisibleForTesting
    synchronized void updatePowerSaveMode(boolean storeIsEmpty, boolean connected) {
        if (!(this.storeIsEmpty == storeIsEmpty && this.connected == connected)) {
            if (storeIsEmpty || !connected) {
                if (this.dispatchPeriodInSeconds > 0) {
                    this.handler.removeMessages(MSG_KEY, MSG_OBJECT);
                }
            }
            if (!storeIsEmpty && connected && this.dispatchPeriodInSeconds > 0) {
                this.handler.sendMessageDelayed(this.handler.obtainMessage(MSG_KEY, MSG_OBJECT), (long) (this.dispatchPeriodInSeconds * GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE));
            }
            StringBuilder append = new StringBuilder().append("PowerSaveMode ");
            String str = (storeIsEmpty || !connected) ? "initiated." : "terminated.";
            Log.iDebug(append.append(str).toString());
            this.storeIsEmpty = storeIsEmpty;
            this.connected = connected;
        }
    }

    public synchronized void updateConnectivityStatus(boolean connected) {
        updatePowerSaveMode(this.storeIsEmpty, connected);
    }
}
