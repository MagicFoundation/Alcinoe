package com.google.analytics.tracking.android;

import android.content.Context;
import android.content.Intent;
import com.google.analytics.tracking.android.AnalyticsGmsCoreClient.OnConnectedListener;
import com.google.analytics.tracking.android.AnalyticsGmsCoreClient.OnConnectionFailedListener;
import com.google.android.gms.analytics.internal.Command;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentLinkedQueue;

class GAServiceProxy implements ServiceProxy, OnConnectedListener, OnConnectionFailedListener {
    private static final long FAILED_CONNECT_WAIT_TIME = 3000;
    private static final int MAX_TRIES = 2;
    private static final long RECONNECT_WAIT_TIME = 5000;
    private static final long SERVICE_CONNECTION_TIMEOUT = 300000;
    private volatile AnalyticsClient client;
    private Clock clock;
    private volatile int connectTries;
    private final Context ctx;
    private volatile Timer disconnectCheckTimer;
    private volatile Timer failedConnectTimer;
    private long idleTimeout;
    private volatile long lastRequestTime;
    private boolean pendingClearHits;
    private boolean pendingDispatch;
    private final Queue<HitParams> queue;
    private volatile Timer reConnectTimer;
    private volatile ConnectState state;
    private AnalyticsStore store;
    private AnalyticsStore testStore;
    private final AnalyticsThread thread;

    /* renamed from: com.google.analytics.tracking.android.GAServiceProxy.3 */
    static /* synthetic */ class AnonymousClass3 {
        static final /* synthetic */ int[] $SwitchMap$com$google$analytics$tracking$android$GAServiceProxy$ConnectState;

        static {
            $SwitchMap$com$google$analytics$tracking$android$GAServiceProxy$ConnectState = new int[ConnectState.values().length];
            try {
                $SwitchMap$com$google$analytics$tracking$android$GAServiceProxy$ConnectState[ConnectState.CONNECTED_LOCAL.ordinal()] = 1;
            } catch (NoSuchFieldError e) {
            }
            try {
                $SwitchMap$com$google$analytics$tracking$android$GAServiceProxy$ConnectState[ConnectState.CONNECTED_SERVICE.ordinal()] = GAServiceProxy.MAX_TRIES;
            } catch (NoSuchFieldError e2) {
            }
            try {
                $SwitchMap$com$google$analytics$tracking$android$GAServiceProxy$ConnectState[ConnectState.DISCONNECTED.ordinal()] = 3;
            } catch (NoSuchFieldError e3) {
            }
        }
    }

    private enum ConnectState {
        CONNECTING,
        CONNECTED_SERVICE,
        CONNECTED_LOCAL,
        BLOCKED,
        PENDING_CONNECTION,
        PENDING_DISCONNECT,
        DISCONNECTED
    }

    private class DisconnectCheckTask extends TimerTask {
        private DisconnectCheckTask() {
        }

        public void run() {
            if (GAServiceProxy.this.state == ConnectState.CONNECTED_SERVICE && GAServiceProxy.this.queue.isEmpty() && GAServiceProxy.this.lastRequestTime + GAServiceProxy.this.idleTimeout < GAServiceProxy.this.clock.currentTimeMillis()) {
                Log.iDebug("Disconnecting due to inactivity");
                GAServiceProxy.this.disconnectFromService();
                return;
            }
            GAServiceProxy.this.disconnectCheckTimer.schedule(new DisconnectCheckTask(), GAServiceProxy.this.idleTimeout);
        }
    }

    private class FailedConnectTask extends TimerTask {
        private FailedConnectTask() {
        }

        public void run() {
            if (GAServiceProxy.this.state == ConnectState.CONNECTING) {
                GAServiceProxy.this.useStore();
            }
        }
    }

    private static class HitParams {
        private final List<Command> commands;
        private final long hitTimeInMilliseconds;
        private final String path;
        private final Map<String, String> wireFormatParams;

        public HitParams(Map<String, String> wireFormatParams, long hitTimeInMilliseconds, String path, List<Command> commands) {
            this.wireFormatParams = wireFormatParams;
            this.hitTimeInMilliseconds = hitTimeInMilliseconds;
            this.path = path;
            this.commands = commands;
        }

        public Map<String, String> getWireFormatParams() {
            return this.wireFormatParams;
        }

        public long getHitTimeInMilliseconds() {
            return this.hitTimeInMilliseconds;
        }

        public String getPath() {
            return this.path;
        }

        public List<Command> getCommands() {
            return this.commands;
        }
    }

    private class ReconnectTask extends TimerTask {
        private ReconnectTask() {
        }

        public void run() {
            GAServiceProxy.this.connectToService();
        }
    }

    GAServiceProxy(Context ctx, AnalyticsThread thread, AnalyticsStore store) {
        this.queue = new ConcurrentLinkedQueue();
        this.idleTimeout = SERVICE_CONNECTION_TIMEOUT;
        this.testStore = store;
        this.ctx = ctx;
        this.thread = thread;
        this.clock = new Clock() {
            public long currentTimeMillis() {
                return System.currentTimeMillis();
            }
        };
        this.connectTries = 0;
        this.state = ConnectState.DISCONNECTED;
    }

    GAServiceProxy(Context ctx, AnalyticsThread thread) {
        this(ctx, thread, null);
    }

    void setClock(Clock clock) {
        this.clock = clock;
    }

    public void putHit(Map<String, String> wireFormatParams, long hitTimeInMilliseconds, String path, List<Command> commands) {
        Log.iDebug("putHit called");
        this.queue.add(new HitParams(wireFormatParams, hitTimeInMilliseconds, path, commands));
        sendQueue();
    }

    public void dispatch() {
        switch (AnonymousClass3.$SwitchMap$com$google$analytics$tracking$android$GAServiceProxy$ConnectState[this.state.ordinal()]) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                dispatchToStore();
            case MAX_TRIES /*2*/:
            default:
                this.pendingDispatch = true;
        }
    }

    public void clearHits() {
        Log.iDebug("clearHits called");
        this.queue.clear();
        switch (AnonymousClass3.$SwitchMap$com$google$analytics$tracking$android$GAServiceProxy$ConnectState[this.state.ordinal()]) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                this.store.clearHits(0);
                this.pendingClearHits = false;
            case MAX_TRIES /*2*/:
                this.client.clearHits();
                this.pendingClearHits = false;
            default:
                this.pendingClearHits = true;
        }
    }

    private Timer cancelTimer(Timer timer) {
        if (timer != null) {
            timer.cancel();
        }
        return null;
    }

    private void clearAllTimers() {
        this.reConnectTimer = cancelTimer(this.reConnectTimer);
        this.failedConnectTimer = cancelTimer(this.failedConnectTimer);
        this.disconnectCheckTimer = cancelTimer(this.disconnectCheckTimer);
    }

    public void createService() {
        if (this.client == null) {
            this.client = new AnalyticsGmsCoreClient(this.ctx, this, this);
            connectToService();
        }
    }

    void createService(AnalyticsClient client) {
        if (this.client == null) {
            this.client = client;
            connectToService();
        }
    }

    public void setIdleTimeout(long idleTimeout) {
        this.idleTimeout = idleTimeout;
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    private synchronized void sendQueue() {
        if (Thread.currentThread().equals(this.thread.getThread())) {
            if (this.pendingClearHits) {
                clearHits();
            }
            switch (AnonymousClass3.$SwitchMap$com$google$analytics$tracking$android$GAServiceProxy$ConnectState[this.state.ordinal()]) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    break;
                case MAX_TRIES /*2*/:
                    while (!this.queue.isEmpty()) {
                        HitParams hitParams = (HitParams) this.queue.peek();
                        Log.iDebug("Sending hit to service");
                        this.client.sendHit(hitParams.getWireFormatParams(), hitParams.getHitTimeInMilliseconds(), hitParams.getPath(), hitParams.getCommands());
                        this.queue.poll();
                    }
                    break;
                case DetectedActivity.STILL /*3*/:
                    Log.iDebug("Need to reconnect");
                    if (!this.queue.isEmpty()) {
                        connectToService();
                        break;
                    }
                    break;
                default:
                    break;
            }
        }
        this.thread.getQueue().add(new Runnable() {
            public void run() {
                GAServiceProxy.this.sendQueue();
            }
        });
    }

    private void dispatchToStore() {
        this.store.dispatch();
        this.pendingDispatch = false;
    }

    private synchronized void useStore() {
        if (this.state != ConnectState.CONNECTED_LOCAL) {
            clearAllTimers();
            Log.iDebug("falling back to local store");
            if (this.testStore != null) {
                this.store = this.testStore;
            } else {
                GAServiceManager instance = GAServiceManager.getInstance();
                instance.initialize(this.ctx, this.thread);
                this.store = instance.getStore();
            }
            this.state = ConnectState.CONNECTED_LOCAL;
            sendQueue();
        }
    }

    private synchronized void connectToService() {
        if (this.client == null || this.state == ConnectState.CONNECTED_LOCAL) {
            Log.w("client not initialized.");
            useStore();
        } else {
            try {
                this.connectTries++;
                cancelTimer(this.failedConnectTimer);
                this.state = ConnectState.CONNECTING;
                this.failedConnectTimer = new Timer("Failed Connect");
                this.failedConnectTimer.schedule(new FailedConnectTask(), FAILED_CONNECT_WAIT_TIME);
                Log.iDebug("connecting to Analytics service");
                this.client.connect();
            } catch (SecurityException e) {
                Log.w("security exception on connectToService");
                useStore();
            }
        }
    }

    private synchronized void disconnectFromService() {
        if (this.client != null && this.state == ConnectState.CONNECTED_SERVICE) {
            this.state = ConnectState.PENDING_DISCONNECT;
            this.client.disconnect();
        }
    }

    public synchronized void onConnected() {
        this.failedConnectTimer = cancelTimer(this.failedConnectTimer);
        this.connectTries = 0;
        Log.iDebug("Connected to service");
        this.state = ConnectState.CONNECTED_SERVICE;
        sendQueue();
        this.disconnectCheckTimer = cancelTimer(this.disconnectCheckTimer);
        this.disconnectCheckTimer = new Timer("disconnect check");
        this.disconnectCheckTimer.schedule(new DisconnectCheckTask(), this.idleTimeout);
    }

    public synchronized void onDisconnected() {
        if (this.state == ConnectState.PENDING_DISCONNECT) {
            Log.iDebug("Disconnected from service");
            clearAllTimers();
            this.state = ConnectState.DISCONNECTED;
        } else {
            Log.iDebug("Unexpected disconnect.");
            this.state = ConnectState.PENDING_CONNECTION;
            if (this.connectTries < MAX_TRIES) {
                fireReconnectAttempt();
            } else {
                useStore();
            }
        }
    }

    public synchronized void onConnectionFailed(int errorCode, Intent resolution) {
        this.state = ConnectState.PENDING_CONNECTION;
        if (this.connectTries < MAX_TRIES) {
            Log.w("Service unavailable (code=" + errorCode + "), will retry.");
            fireReconnectAttempt();
        } else {
            Log.w("Service unavailable (code=" + errorCode + "), using local store.");
            useStore();
        }
    }

    private void fireReconnectAttempt() {
        this.reConnectTimer = cancelTimer(this.reConnectTimer);
        this.reConnectTimer = new Timer("Service Reconnect");
        this.reConnectTimer.schedule(new ReconnectTask(), RECONNECT_WAIT_TIME);
    }
}
