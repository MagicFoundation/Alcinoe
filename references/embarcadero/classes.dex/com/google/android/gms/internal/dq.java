package com.google.android.gms.internal;

import android.content.Context;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Looper;
import android.os.RemoteException;
import android.support.v4.media.TransportMediator;
import android.text.TextUtils;
import com.google.android.gms.cast.ApplicationMetadata;
import com.google.android.gms.cast.Cast;
import com.google.android.gms.cast.Cast.ApplicationConnectionResult;
import com.google.android.gms.cast.Cast.Listener;
import com.google.android.gms.cast.Cast.MessageReceivedCallback;
import com.google.android.gms.cast.CastDevice;
import com.google.android.gms.cast.CastStatusCodes;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.common.api.a.c;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.internal.eh.e;
import com.google.android.gms.location.GeofenceStatusCodes;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

public final class dq extends eh<ds> {
    private static final du xE;
    private static final Object xU;
    private static final Object xV;
    private final Handler mHandler;
    private final Listener wz;
    private ApplicationMetadata xF;
    private final CastDevice xG;
    private final dt xH;
    private final Map<String, MessageReceivedCallback> xI;
    private final long xJ;
    private String xK;
    private boolean xL;
    private boolean xM;
    private final AtomicLong xN;
    private String xO;
    private String xP;
    private Bundle xQ;
    private Map<Long, c<Status>> xR;
    private c<ApplicationConnectionResult> xS;
    private c<Status> xT;
    private double xe;
    private boolean xf;

    private static final class a implements ApplicationConnectionResult {
        private final String pz;
        private final Status vl;
        private final ApplicationMetadata yb;
        private final String yc;
        private final boolean yd;

        public a(Status status) {
            this(status, null, null, null, false);
        }

        public a(Status status, ApplicationMetadata applicationMetadata, String str, String str2, boolean z) {
            this.vl = status;
            this.yb = applicationMetadata;
            this.yc = str;
            this.pz = str2;
            this.yd = z;
        }

        public ApplicationMetadata getApplicationMetadata() {
            return this.yb;
        }

        public String getApplicationStatus() {
            return this.yc;
        }

        public String getSessionId() {
            return this.pz;
        }

        public Status getStatus() {
            return this.vl;
        }

        public boolean getWasLaunched() {
            return this.yd;
        }
    }

    static {
        xE = new du("CastClientImpl");
        xU = new Object();
        xV = new Object();
    }

    public dq(Context context, Looper looper, CastDevice castDevice, long j, Listener listener, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
        super(context, looper, connectionCallbacks, onConnectionFailedListener, (String[]) null);
        this.xG = castDevice;
        this.wz = listener;
        this.xJ = j;
        this.mHandler = new Handler(looper);
        this.xI = new HashMap();
        this.xM = false;
        this.xF = null;
        this.xK = null;
        this.xe = 0.0d;
        this.xf = false;
        this.xN = new AtomicLong(0);
        this.xR = new HashMap();
        this.xH = new com.google.android.gms.internal.dt.a() {
            final /* synthetic */ dq xW;

            /* renamed from: com.google.android.gms.internal.dq.1.1 */
            class AnonymousClass1 implements Runnable {
                final /* synthetic */ int xX;
                final /* synthetic */ AnonymousClass1 xY;

                AnonymousClass1(AnonymousClass1 anonymousClass1, int i) {
                    this.xY = anonymousClass1;
                    this.xX = i;
                }

                public void run() {
                    if (this.xY.xW.wz != null) {
                        this.xY.xW.wz.onApplicationDisconnected(this.xX);
                    }
                }
            }

            /* renamed from: com.google.android.gms.internal.dq.1.2 */
            class AnonymousClass2 implements Runnable {
                final /* synthetic */ AnonymousClass1 xY;
                final /* synthetic */ String xZ;
                final /* synthetic */ double xs;
                final /* synthetic */ boolean xt;

                AnonymousClass2(AnonymousClass1 anonymousClass1, String str, double d, boolean z) {
                    this.xY = anonymousClass1;
                    this.xZ = str;
                    this.xs = d;
                    this.xt = z;
                }

                public void run() {
                    this.xY.xW.a(this.xZ, this.xs, this.xt);
                }
            }

            /* renamed from: com.google.android.gms.internal.dq.1.3 */
            class AnonymousClass3 implements Runnable {
                final /* synthetic */ String wp;
                final /* synthetic */ AnonymousClass1 xY;
                final /* synthetic */ String ya;

                AnonymousClass3(AnonymousClass1 anonymousClass1, String str, String str2) {
                    this.xY = anonymousClass1;
                    this.wp = str;
                    this.ya = str2;
                }

                public void run() {
                    synchronized (this.xY.xW.xI) {
                        MessageReceivedCallback messageReceivedCallback = (MessageReceivedCallback) this.xY.xW.xI.get(this.wp);
                    }
                    if (messageReceivedCallback != null) {
                        messageReceivedCallback.onMessageReceived(this.xY.xW.xG, this.wp, this.ya);
                        return;
                    }
                    dq.xE.b("Discarded message for unknown namespace '%s'", this.wp);
                }
            }

            {
                this.xW = r1;
            }

            private boolean D(int i) {
                synchronized (dq.xV) {
                    if (this.xW.xT != null) {
                        this.xW.xT.b(new Status(i));
                        this.xW.xT = null;
                        return true;
                    }
                    return false;
                }
            }

            private void b(long j, int i) {
                synchronized (this.xW.xR) {
                    c cVar = (c) this.xW.xR.remove(Long.valueOf(j));
                }
                if (cVar != null) {
                    cVar.b(new Status(i));
                }
            }

            public void A(int i) {
                synchronized (dq.xU) {
                    if (this.xW.xS != null) {
                        this.xW.xS.b(new a(new Status(i)));
                        this.xW.xS = null;
                    }
                }
            }

            public void B(int i) {
                D(i);
            }

            public void C(int i) {
                D(i);
            }

            public void a(ApplicationMetadata applicationMetadata, String str, String str2, boolean z) {
                this.xW.xF = applicationMetadata;
                this.xW.xO = applicationMetadata.getApplicationId();
                this.xW.xP = str2;
                synchronized (dq.xU) {
                    if (this.xW.xS != null) {
                        this.xW.xS.b(new a(new Status(0), applicationMetadata, str, str2, z));
                        this.xW.xS = null;
                    }
                }
            }

            public void a(String str, long j) {
                b(j, 0);
            }

            public void a(String str, long j, int i) {
                b(j, i);
            }

            public void b(String str, double d, boolean z) {
                this.xW.mHandler.post(new AnonymousClass2(this, str, d, z));
            }

            public void b(String str, byte[] bArr) {
                dq.xE.b("IGNORING: Receive (type=binary, ns=%s) <%d bytes>", str, Integer.valueOf(bArr.length));
            }

            public void d(String str, String str2) {
                dq.xE.b("Receive (type=text, ns=%s) %s", str, str2);
                this.xW.mHandler.post(new AnonymousClass3(this, str, str2));
            }

            public void onApplicationDisconnected(int statusCode) {
                this.xW.xO = null;
                this.xW.xP = null;
                if (!D(statusCode) && this.xW.wz != null) {
                    this.xW.mHandler.post(new AnonymousClass1(this, statusCode));
                }
            }

            public void z(int i) {
                dq.xE.b("ICastDeviceControllerListener.onDisconnected: %d", Integer.valueOf(i));
                this.xW.xM = false;
                this.xW.xF = null;
                if (i != 0) {
                    this.xW.O(2);
                }
            }
        };
    }

    private void a(String str, double d, boolean z) {
        boolean z2;
        if (dr.a(str, this.xK)) {
            z2 = false;
        } else {
            this.xK = str;
            int i = 1;
        }
        if (this.wz != null && (r0 != 0 || this.xL)) {
            this.wz.onApplicationStatusChanged();
        }
        if (d != this.xe) {
            this.xe = d;
            z2 = true;
        } else {
            z2 = false;
        }
        if (z != this.xf) {
            this.xf = z;
            z2 = true;
        }
        xE.b("hasChange=%b, mFirstStatusUpdate=%b", Boolean.valueOf(z2), Boolean.valueOf(this.xL));
        if (this.wz != null && (z2 || this.xL)) {
            this.wz.onVolumeChanged();
        }
        this.xL = false;
    }

    private void d(c<ApplicationConnectionResult> cVar) {
        synchronized (xU) {
            if (this.xS != null) {
                this.xS.b(new a(new Status(CastStatusCodes.CANCELED)));
            }
            this.xS = cVar;
        }
    }

    private void db() throws IllegalStateException {
        if (!this.xM) {
            throw new IllegalStateException("not connected to a device");
        }
    }

    private void f(c<Status> cVar) {
        synchronized (xV) {
            if (this.xT != null) {
                cVar.b(new Status(GamesStatusCodes.STATUS_REQUEST_UPDATE_TOTAL_FAILURE));
                return;
            }
            this.xT = cVar;
        }
    }

    public void Q(String str) throws IllegalArgumentException, RemoteException {
        if (TextUtils.isEmpty(str)) {
            throw new IllegalArgumentException("Channel namespace cannot be null or empty");
        }
        synchronized (this.xI) {
            MessageReceivedCallback messageReceivedCallback = (MessageReceivedCallback) this.xI.remove(str);
        }
        if (messageReceivedCallback != null) {
            try {
                ((ds) eb()).T(str);
            } catch (Throwable e) {
                xE.a(e, "Error unregistering namespace (%s): %s", str, e.getMessage());
            }
        }
    }

    public void a(double d) throws IllegalArgumentException, IllegalStateException, RemoteException {
        if (Double.isInfinite(d) || Double.isNaN(d)) {
            throw new IllegalArgumentException("Volume cannot be " + d);
        }
        ((ds) eb()).a(d, this.xe, this.xf);
    }

    protected void a(int i, IBinder iBinder, Bundle bundle) {
        if (i == 0 || i == GeofenceStatusCodes.GEOFENCE_TOO_MANY_GEOFENCES) {
            this.xM = true;
            this.xL = true;
        } else {
            this.xM = false;
        }
        if (i == GeofenceStatusCodes.GEOFENCE_TOO_MANY_GEOFENCES) {
            this.xQ = new Bundle();
            this.xQ.putBoolean(Cast.EXTRA_APP_NO_LONGER_RUNNING, true);
            i = 0;
        }
        super.a(i, iBinder, bundle);
    }

    protected void a(en enVar, e eVar) throws RemoteException {
        Bundle bundle = new Bundle();
        xE.b("getServiceFromBroker(): mLastApplicationId=%s, mLastSessionId=%s", this.xO, this.xP);
        this.xG.putInBundle(bundle);
        bundle.putLong("com.google.android.gms.cast.EXTRA_CAST_FLAGS", this.xJ);
        if (this.xO != null) {
            bundle.putString("last_application_id", this.xO);
            if (this.xP != null) {
                bundle.putString("last_session_id", this.xP);
            }
        }
        enVar.a((em) eVar, (int) GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_VERSION_CODE, getContext().getPackageName(), this.xH.asBinder(), bundle);
    }

    public void a(String str, MessageReceivedCallback messageReceivedCallback) throws IllegalArgumentException, IllegalStateException, RemoteException {
        if (TextUtils.isEmpty(str)) {
            throw new IllegalArgumentException("Channel namespace cannot be null or empty");
        }
        Q(str);
        if (messageReceivedCallback != null) {
            synchronized (this.xI) {
                this.xI.put(str, messageReceivedCallback);
            }
            ((ds) eb()).S(str);
        }
    }

    public void a(String str, c<Status> cVar) throws IllegalStateException, RemoteException {
        f((c) cVar);
        ((ds) eb()).R(str);
    }

    public void a(String str, String str2, c<Status> cVar) throws IllegalArgumentException, IllegalStateException, RemoteException {
        if (TextUtils.isEmpty(str2)) {
            throw new IllegalArgumentException("The message payload cannot be null or empty");
        } else if (str == null || str.length() > TransportMediator.FLAG_KEY_MEDIA_NEXT) {
            throw new IllegalArgumentException("Invalid namespace length");
        } else if (str2.length() > Cast.MAX_MESSAGE_LENGTH) {
            throw new IllegalArgumentException("Message exceeds maximum size");
        } else {
            db();
            long incrementAndGet = this.xN.incrementAndGet();
            ((ds) eb()).a(str, str2, incrementAndGet);
            this.xR.put(Long.valueOf(incrementAndGet), cVar);
        }
    }

    public void a(String str, boolean z, c<ApplicationConnectionResult> cVar) throws IllegalStateException, RemoteException {
        d((c) cVar);
        ((ds) eb()).e(str, z);
    }

    protected String aF() {
        return "com.google.android.gms.cast.service.BIND_CAST_DEVICE_CONTROLLER_SERVICE";
    }

    protected String aG() {
        return "com.google.android.gms.cast.internal.ICastDeviceController";
    }

    public void b(String str, String str2, c<ApplicationConnectionResult> cVar) throws IllegalStateException, RemoteException {
        d((c) cVar);
        ((ds) eb()).e(str, str2);
    }

    public Bundle cY() {
        if (this.xQ == null) {
            return super.cY();
        }
        Bundle bundle = this.xQ;
        this.xQ = null;
        return bundle;
    }

    public void cZ() throws IllegalStateException, RemoteException {
        ((ds) eb()).cZ();
    }

    public double da() throws IllegalStateException {
        db();
        return this.xe;
    }

    public void disconnect() {
        try {
            if (isConnected()) {
                synchronized (this.xI) {
                    this.xI.clear();
                }
                ((ds) eb()).disconnect();
            }
        } catch (RemoteException e) {
            try {
                xE.b("Error while disconnecting the controller interface: %s", e.getMessage());
            } catch (Throwable th) {
                super.disconnect();
            }
        }
        super.disconnect();
    }

    public void e(c<Status> cVar) throws IllegalStateException, RemoteException {
        f((c) cVar);
        ((ds) eb()).df();
    }

    public ApplicationMetadata getApplicationMetadata() throws IllegalStateException {
        db();
        return this.xF;
    }

    public String getApplicationStatus() throws IllegalStateException {
        db();
        return this.xK;
    }

    public boolean isMute() throws IllegalStateException {
        db();
        return this.xf;
    }

    protected /* synthetic */ IInterface p(IBinder iBinder) {
        return v(iBinder);
    }

    public void t(boolean z) throws IllegalStateException, RemoteException {
        ((ds) eb()).a(z, this.xe, this.xf);
    }

    protected ds v(IBinder iBinder) {
        return com.google.android.gms.internal.ds.a.w(iBinder);
    }
}
