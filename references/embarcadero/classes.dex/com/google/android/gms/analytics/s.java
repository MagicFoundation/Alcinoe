package com.google.android.gms.analytics;

import android.content.Context;
import android.content.Intent;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.internal.di;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentLinkedQueue;

class s implements ag, com.google.android.gms.analytics.c.b, com.google.android.gms.analytics.c.c {
    private final Context mContext;
    private final GoogleAnalytics rA;
    private final Queue<d> rB;
    private volatile int rC;
    private volatile Timer rD;
    private volatile Timer rE;
    private volatile Timer rF;
    private boolean rG;
    private boolean rH;
    private boolean rI;
    private i rJ;
    private long rK;
    private d rj;
    private final f rk;
    private boolean rm;
    private volatile long rw;
    private volatile a rx;
    private volatile b ry;
    private d rz;

    /* renamed from: com.google.android.gms.analytics.s.3 */
    static /* synthetic */ class AnonymousClass3 {
        static final /* synthetic */ int[] rM;

        static {
            rM = new int[a.values().length];
            try {
                rM[a.CONNECTED_LOCAL.ordinal()] = 1;
            } catch (NoSuchFieldError e) {
            }
            try {
                rM[a.CONNECTED_SERVICE.ordinal()] = 2;
            } catch (NoSuchFieldError e2) {
            }
            try {
                rM[a.CONNECTING.ordinal()] = 3;
            } catch (NoSuchFieldError e3) {
            }
            try {
                rM[a.PENDING_CONNECTION.ordinal()] = 4;
            } catch (NoSuchFieldError e4) {
            }
            try {
                rM[a.PENDING_DISCONNECT.ordinal()] = 5;
            } catch (NoSuchFieldError e5) {
            }
            try {
                rM[a.DISCONNECTED.ordinal()] = 6;
            } catch (NoSuchFieldError e6) {
            }
        }
    }

    private enum a {
        CONNECTING,
        CONNECTED_SERVICE,
        CONNECTED_LOCAL,
        BLOCKED,
        PENDING_CONNECTION,
        PENDING_DISCONNECT,
        DISCONNECTED
    }

    private class b extends TimerTask {
        final /* synthetic */ s rL;

        private b(s sVar) {
            this.rL = sVar;
        }

        public void run() {
            if (this.rL.rx == a.CONNECTED_SERVICE && this.rL.rB.isEmpty() && this.rL.rw + this.rL.rK < this.rL.rJ.currentTimeMillis()) {
                aa.v("Disconnecting due to inactivity");
                this.rL.aD();
                return;
            }
            this.rL.rF.schedule(new b(this.rL), this.rL.rK);
        }
    }

    private class c extends TimerTask {
        final /* synthetic */ s rL;

        private c(s sVar) {
            this.rL = sVar;
        }

        public void run() {
            if (this.rL.rx == a.CONNECTING) {
                this.rL.bL();
            }
        }
    }

    private static class d {
        private final Map<String, String> rV;
        private final long rW;
        private final String rX;
        private final List<di> rY;

        public d(Map<String, String> map, long j, String str, List<di> list) {
            this.rV = map;
            this.rW = j;
            this.rX = str;
            this.rY = list;
        }

        public Map<String, String> bO() {
            return this.rV;
        }

        public long bP() {
            return this.rW;
        }

        public List<di> bQ() {
            return this.rY;
        }

        public String getPath() {
            return this.rX;
        }

        public String toString() {
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.append("PATH: ");
            stringBuilder.append(this.rX);
            if (this.rV != null) {
                stringBuilder.append("  PARAMS: ");
                for (Entry entry : this.rV.entrySet()) {
                    stringBuilder.append((String) entry.getKey());
                    stringBuilder.append("=");
                    stringBuilder.append((String) entry.getValue());
                    stringBuilder.append(",  ");
                }
            }
            return stringBuilder.toString();
        }
    }

    private class e extends TimerTask {
        final /* synthetic */ s rL;

        private e(s sVar) {
            this.rL = sVar;
        }

        public void run() {
            this.rL.bM();
        }
    }

    s(Context context, f fVar) {
        this(context, fVar, null, GoogleAnalytics.getInstance(context));
    }

    s(Context context, f fVar, d dVar, GoogleAnalytics googleAnalytics) {
        this.rB = new ConcurrentLinkedQueue();
        this.rK = 300000;
        this.rz = dVar;
        this.mContext = context;
        this.rk = fVar;
        this.rA = googleAnalytics;
        this.rJ = new i() {
            final /* synthetic */ s rL;

            {
                this.rL = r1;
            }

            public long currentTimeMillis() {
                return System.currentTimeMillis();
            }
        };
        this.rC = 0;
        this.rx = a.DISCONNECTED;
    }

    private Timer a(Timer timer) {
        if (timer != null) {
            timer.cancel();
        }
        return null;
    }

    private synchronized void aD() {
        if (this.ry != null && this.rx == a.CONNECTED_SERVICE) {
            this.rx = a.PENDING_DISCONNECT;
            this.ry.disconnect();
        }
    }

    private void bH() {
        this.rD = a(this.rD);
        this.rE = a(this.rE);
        this.rF = a(this.rF);
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    private synchronized void bJ() {
        if (Thread.currentThread().equals(this.rk.getThread())) {
            if (this.rG) {
                bk();
            }
            switch (AnonymousClass3.rM[this.rx.ordinal()]) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    while (!this.rB.isEmpty()) {
                        d dVar = (d) this.rB.peek();
                        aa.v("Sending hit to service   " + dVar);
                        if (this.rA.isDryRunEnabled()) {
                            aa.v("Dry run enabled. Hit not actually sent to service.");
                        } else {
                            this.ry.a(dVar.bO(), dVar.bP(), dVar.getPath(), dVar.bQ());
                        }
                        this.rB.poll();
                    }
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    aa.v("Need to reconnect");
                    if (!this.rB.isEmpty()) {
                        bM();
                        break;
                    }
                    break;
                default:
                    break;
            }
        }
        this.rk.bs().add(new Runnable() {
            final /* synthetic */ s rL;

            {
                this.rL = r1;
            }

            public void run() {
                this.rL.bJ();
            }
        });
    }

    private void bK() {
        this.rj.bp();
        this.rm = false;
    }

    private synchronized void bL() {
        if (this.rx != a.CONNECTED_LOCAL) {
            bH();
            aa.v("falling back to local store");
            if (this.rz != null) {
                this.rj = this.rz;
            } else {
                r bB = r.bB();
                bB.a(this.mContext, this.rk);
                this.rj = bB.bE();
            }
            this.rx = a.CONNECTED_LOCAL;
            bJ();
        }
    }

    private synchronized void bM() {
        if (this.rI || this.ry == null || this.rx == a.CONNECTED_LOCAL) {
            aa.w("client not initialized.");
            bL();
        } else {
            try {
                this.rC++;
                a(this.rE);
                this.rx = a.CONNECTING;
                this.rE = new Timer("Failed Connect");
                this.rE.schedule(new c(), 3000);
                aa.v("connecting to Analytics service");
                this.ry.connect();
            } catch (SecurityException e) {
                aa.w("security exception on connectToService");
                bL();
            }
        }
    }

    private void bN() {
        this.rD = a(this.rD);
        this.rD = new Timer("Service Reconnect");
        this.rD.schedule(new e(), 5000);
    }

    public synchronized void a(int i, Intent intent) {
        this.rx = a.PENDING_CONNECTION;
        if (this.rC < 2) {
            aa.w("Service unavailable (code=" + i + "), will retry.");
            bN();
        } else {
            aa.w("Service unavailable (code=" + i + "), using local store.");
            bL();
        }
    }

    public void b(Map<String, String> map, long j, String str, List<di> list) {
        aa.v("putHit called");
        this.rB.add(new d(map, j, str, list));
        bJ();
    }

    public void bI() {
        if (this.ry == null) {
            this.ry = new c(this.mContext, this, this);
            bM();
        }
    }

    public void bk() {
        aa.v("clearHits called");
        this.rB.clear();
        switch (AnonymousClass3.rM[this.rx.ordinal()]) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                this.rj.i(0);
                this.rG = false;
            case DetectedActivity.ON_FOOT /*2*/:
                this.ry.bk();
                this.rG = false;
            default:
                this.rG = true;
        }
    }

    public void bp() {
        switch (AnonymousClass3.rM[this.rx.ordinal()]) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                bK();
            case DetectedActivity.ON_FOOT /*2*/:
            default:
                this.rm = true;
        }
    }

    public synchronized void br() {
        if (!this.rI) {
            aa.v("setForceLocalDispatch called.");
            this.rI = true;
            switch (AnonymousClass3.rM[this.rx.ordinal()]) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                case DetectedActivity.UNKNOWN /*4*/:
                case DetectedActivity.TILTING /*5*/:
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    aD();
                    break;
                case DetectedActivity.STILL /*3*/:
                    this.rH = true;
                    break;
                default:
                    break;
            }
        }
    }

    public synchronized void onConnected() {
        this.rE = a(this.rE);
        this.rC = 0;
        aa.v("Connected to service");
        this.rx = a.CONNECTED_SERVICE;
        if (this.rH) {
            aD();
            this.rH = false;
        } else {
            bJ();
            this.rF = a(this.rF);
            this.rF = new Timer("disconnect check");
            this.rF.schedule(new b(), this.rK);
        }
    }

    public synchronized void onDisconnected() {
        if (this.rx == a.PENDING_DISCONNECT) {
            aa.v("Disconnected from service");
            bH();
            this.rx = a.DISCONNECTED;
        } else {
            aa.v("Unexpected disconnect.");
            this.rx = a.PENDING_CONNECTION;
            if (this.rC < 2) {
                bN();
            } else {
                bL();
            }
        }
    }
}
