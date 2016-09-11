package com.google.android.gms.cast;

import android.content.Context;
import android.os.Looper;
import android.os.RemoteException;
import android.text.TextUtils;
import com.google.android.gms.common.api.Api;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.GoogleApiClient.ApiOptions;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Scope;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.internal.dq;
import com.google.android.gms.internal.ee;
import com.google.android.gms.internal.er;
import java.io.IOException;

public final class Cast {
    public static final Api API;
    public static final CastApi CastApi;
    public static final String EXTRA_APP_NO_LONGER_RUNNING = "com.google.android.gms.cast.EXTRA_APP_NO_LONGER_RUNNING";
    public static final int MAX_MESSAGE_LENGTH = 65536;
    public static final int MAX_NAMESPACE_LENGTH = 128;
    static final com.google.android.gms.common.api.Api.b<dq> va;

    public interface CastApi {

        public static final class a implements CastApi {

            /* renamed from: com.google.android.gms.cast.Cast.CastApi.a.1 */
            class AnonymousClass1 extends b {
                final /* synthetic */ String wp;
                final /* synthetic */ String wq;
                final /* synthetic */ a wr;

                AnonymousClass1(a aVar, String str, String str2) {
                    this.wr = aVar;
                    this.wp = str;
                    this.wq = str2;
                    super();
                }

                protected void a(dq dqVar) throws RemoteException {
                    try {
                        dqVar.a(this.wp, this.wq, (com.google.android.gms.common.api.a.c) this);
                    } catch (IllegalArgumentException e) {
                        x(GamesStatusCodes.STATUS_REQUEST_UPDATE_TOTAL_FAILURE);
                    } catch (IllegalStateException e2) {
                        x(GamesStatusCodes.STATUS_REQUEST_UPDATE_TOTAL_FAILURE);
                    }
                }
            }

            /* renamed from: com.google.android.gms.cast.Cast.CastApi.a.2 */
            class AnonymousClass2 extends c {
                final /* synthetic */ a wr;
                final /* synthetic */ String ws;

                AnonymousClass2(a aVar, String str) {
                    this.wr = aVar;
                    this.ws = str;
                    super();
                }

                protected void a(dq dqVar) throws RemoteException {
                    try {
                        dqVar.a(this.ws, false, (com.google.android.gms.common.api.a.c) this);
                    } catch (IllegalStateException e) {
                        x(GamesStatusCodes.STATUS_REQUEST_UPDATE_TOTAL_FAILURE);
                    }
                }
            }

            /* renamed from: com.google.android.gms.cast.Cast.CastApi.a.3 */
            class AnonymousClass3 extends c {
                final /* synthetic */ a wr;
                final /* synthetic */ String ws;
                final /* synthetic */ boolean wt;

                AnonymousClass3(a aVar, String str, boolean z) {
                    this.wr = aVar;
                    this.ws = str;
                    this.wt = z;
                    super();
                }

                protected void a(dq dqVar) throws RemoteException {
                    try {
                        dqVar.a(this.ws, this.wt, (com.google.android.gms.common.api.a.c) this);
                    } catch (IllegalStateException e) {
                        x(GamesStatusCodes.STATUS_REQUEST_UPDATE_TOTAL_FAILURE);
                    }
                }
            }

            /* renamed from: com.google.android.gms.cast.Cast.CastApi.a.4 */
            class AnonymousClass4 extends c {
                final /* synthetic */ a wr;
                final /* synthetic */ String ws;
                final /* synthetic */ String wu;

                AnonymousClass4(a aVar, String str, String str2) {
                    this.wr = aVar;
                    this.ws = str;
                    this.wu = str2;
                    super();
                }

                protected void a(dq dqVar) throws RemoteException {
                    try {
                        dqVar.b(this.ws, this.wu, this);
                    } catch (IllegalStateException e) {
                        x(GamesStatusCodes.STATUS_REQUEST_UPDATE_TOTAL_FAILURE);
                    }
                }
            }

            /* renamed from: com.google.android.gms.cast.Cast.CastApi.a.5 */
            class AnonymousClass5 extends c {
                final /* synthetic */ a wr;
                final /* synthetic */ String ws;

                AnonymousClass5(a aVar, String str) {
                    this.wr = aVar;
                    this.ws = str;
                    super();
                }

                protected void a(dq dqVar) throws RemoteException {
                    try {
                        dqVar.b(this.ws, null, this);
                    } catch (IllegalStateException e) {
                        x(GamesStatusCodes.STATUS_REQUEST_UPDATE_TOTAL_FAILURE);
                    }
                }
            }

            /* renamed from: com.google.android.gms.cast.Cast.CastApi.a.9 */
            class AnonymousClass9 extends b {
                final /* synthetic */ a wr;
                final /* synthetic */ String wu;

                AnonymousClass9(a aVar, String str) {
                    this.wr = aVar;
                    this.wu = str;
                    super();
                }

                protected void a(dq dqVar) throws RemoteException {
                    if (TextUtils.isEmpty(this.wu)) {
                        c(GamesStatusCodes.STATUS_REQUEST_UPDATE_TOTAL_FAILURE, "IllegalArgument: sessionId cannot be null or empty");
                        return;
                    }
                    try {
                        dqVar.a(this.wu, (com.google.android.gms.common.api.a.c) this);
                    } catch (IllegalStateException e) {
                        x(GamesStatusCodes.STATUS_REQUEST_UPDATE_TOTAL_FAILURE);
                    }
                }
            }

            public ApplicationMetadata getApplicationMetadata(GoogleApiClient client) throws IllegalStateException {
                return ((dq) client.a(Cast.va)).getApplicationMetadata();
            }

            public String getApplicationStatus(GoogleApiClient client) throws IllegalStateException {
                return ((dq) client.a(Cast.va)).getApplicationStatus();
            }

            public double getVolume(GoogleApiClient client) throws IllegalStateException {
                return ((dq) client.a(Cast.va)).da();
            }

            public boolean isMute(GoogleApiClient client) throws IllegalStateException {
                return ((dq) client.a(Cast.va)).isMute();
            }

            public PendingResult<ApplicationConnectionResult> joinApplication(GoogleApiClient client) {
                return client.b(new c() {
                    final /* synthetic */ a wr;

                    {
                        this.wr = r2;
                    }

                    protected void a(dq dqVar) throws RemoteException {
                        try {
                            dqVar.b(null, null, this);
                        } catch (IllegalStateException e) {
                            x(GamesStatusCodes.STATUS_REQUEST_UPDATE_TOTAL_FAILURE);
                        }
                    }
                });
            }

            public PendingResult<ApplicationConnectionResult> joinApplication(GoogleApiClient client, String applicationId) {
                return client.b(new AnonymousClass5(this, applicationId));
            }

            public PendingResult<ApplicationConnectionResult> joinApplication(GoogleApiClient client, String applicationId, String sessionId) {
                return client.b(new AnonymousClass4(this, applicationId, sessionId));
            }

            public PendingResult<ApplicationConnectionResult> launchApplication(GoogleApiClient client, String applicationId) {
                return client.b(new AnonymousClass2(this, applicationId));
            }

            public PendingResult<ApplicationConnectionResult> launchApplication(GoogleApiClient client, String applicationId, boolean relaunchIfRunning) {
                return client.b(new AnonymousClass3(this, applicationId, relaunchIfRunning));
            }

            public PendingResult<Status> leaveApplication(GoogleApiClient client) {
                return client.b(new b() {
                    final /* synthetic */ a wr;

                    {
                        this.wr = r2;
                    }

                    protected void a(dq dqVar) throws RemoteException {
                        try {
                            dqVar.e((com.google.android.gms.common.api.a.c) this);
                        } catch (IllegalStateException e) {
                            x(GamesStatusCodes.STATUS_REQUEST_UPDATE_TOTAL_FAILURE);
                        }
                    }
                });
            }

            public void removeMessageReceivedCallbacks(GoogleApiClient client, String namespace) throws IOException, IllegalArgumentException {
                try {
                    ((dq) client.a(Cast.va)).Q(namespace);
                } catch (RemoteException e) {
                    throw new IOException("service error");
                }
            }

            public void requestStatus(GoogleApiClient client) throws IOException, IllegalStateException {
                try {
                    ((dq) client.a(Cast.va)).cZ();
                } catch (RemoteException e) {
                    throw new IOException("service error");
                }
            }

            public PendingResult<Status> sendMessage(GoogleApiClient client, String namespace, String message) {
                return client.b(new AnonymousClass1(this, namespace, message));
            }

            public void setMessageReceivedCallbacks(GoogleApiClient client, String namespace, MessageReceivedCallback callbacks) throws IOException, IllegalStateException {
                try {
                    ((dq) client.a(Cast.va)).a(namespace, callbacks);
                } catch (RemoteException e) {
                    throw new IOException("service error");
                }
            }

            public void setMute(GoogleApiClient client, boolean mute) throws IOException, IllegalStateException {
                try {
                    ((dq) client.a(Cast.va)).t(mute);
                } catch (RemoteException e) {
                    throw new IOException("service error");
                }
            }

            public void setVolume(GoogleApiClient client, double volume) throws IOException, IllegalArgumentException, IllegalStateException {
                try {
                    ((dq) client.a(Cast.va)).a(volume);
                } catch (RemoteException e) {
                    throw new IOException("service error");
                }
            }

            public PendingResult<Status> stopApplication(GoogleApiClient client) {
                return client.b(new b() {
                    final /* synthetic */ a wr;

                    {
                        this.wr = r2;
                    }

                    protected void a(dq dqVar) throws RemoteException {
                        try {
                            dqVar.a("", (com.google.android.gms.common.api.a.c) this);
                        } catch (IllegalStateException e) {
                            x(GamesStatusCodes.STATUS_REQUEST_UPDATE_TOTAL_FAILURE);
                        }
                    }
                });
            }

            public PendingResult<Status> stopApplication(GoogleApiClient client, String sessionId) {
                return client.b(new AnonymousClass9(this, sessionId));
            }
        }

        ApplicationMetadata getApplicationMetadata(GoogleApiClient googleApiClient) throws IllegalStateException;

        String getApplicationStatus(GoogleApiClient googleApiClient) throws IllegalStateException;

        double getVolume(GoogleApiClient googleApiClient) throws IllegalStateException;

        boolean isMute(GoogleApiClient googleApiClient) throws IllegalStateException;

        PendingResult<ApplicationConnectionResult> joinApplication(GoogleApiClient googleApiClient);

        PendingResult<ApplicationConnectionResult> joinApplication(GoogleApiClient googleApiClient, String str);

        PendingResult<ApplicationConnectionResult> joinApplication(GoogleApiClient googleApiClient, String str, String str2);

        PendingResult<ApplicationConnectionResult> launchApplication(GoogleApiClient googleApiClient, String str);

        PendingResult<ApplicationConnectionResult> launchApplication(GoogleApiClient googleApiClient, String str, boolean z);

        PendingResult<Status> leaveApplication(GoogleApiClient googleApiClient);

        void removeMessageReceivedCallbacks(GoogleApiClient googleApiClient, String str) throws IOException, IllegalArgumentException;

        void requestStatus(GoogleApiClient googleApiClient) throws IOException, IllegalStateException;

        PendingResult<Status> sendMessage(GoogleApiClient googleApiClient, String str, String str2);

        void setMessageReceivedCallbacks(GoogleApiClient googleApiClient, String str, MessageReceivedCallback messageReceivedCallback) throws IOException, IllegalStateException;

        void setMute(GoogleApiClient googleApiClient, boolean z) throws IOException, IllegalStateException;

        void setVolume(GoogleApiClient googleApiClient, double d) throws IOException, IllegalArgumentException, IllegalStateException;

        PendingResult<Status> stopApplication(GoogleApiClient googleApiClient);

        PendingResult<Status> stopApplication(GoogleApiClient googleApiClient, String str);
    }

    public static abstract class Listener {
        public void onApplicationDisconnected(int statusCode) {
        }

        public void onApplicationStatusChanged() {
        }

        public void onVolumeChanged() {
        }
    }

    public interface MessageReceivedCallback {
        void onMessageReceived(CastDevice castDevice, String str, String str2);
    }

    public interface ApplicationConnectionResult extends Result {
        ApplicationMetadata getApplicationMetadata();

        String getApplicationStatus();

        String getSessionId();

        boolean getWasLaunched();
    }

    public static final class CastOptions implements ApiOptions {
        final CastDevice wv;
        final Listener ww;
        private final int wx;

        public static final class Builder {
            private int wA;
            CastDevice wy;
            Listener wz;

            private Builder(CastDevice castDevice, Listener castListener) {
                er.b((Object) castDevice, (Object) "CastDevice parameter cannot be null");
                er.b((Object) castListener, (Object) "CastListener parameter cannot be null");
                this.wy = castDevice;
                this.wz = castListener;
                this.wA = 0;
            }

            public CastOptions build() {
                return new CastOptions();
            }

            public Builder setVerboseLoggingEnabled(boolean enabled) {
                if (enabled) {
                    this.wA |= 1;
                } else {
                    this.wA &= -2;
                }
                return this;
            }
        }

        private CastOptions(Builder builder) {
            this.wv = builder.wy;
            this.ww = builder.wz;
            this.wx = builder.wA;
        }

        public static Builder builder(CastDevice castDevice, Listener castListener) {
            return new Builder(castListener, null);
        }
    }

    protected static abstract class a<R extends Result> extends com.google.android.gms.common.api.a.a<R, dq> implements PendingResult<R> {
        public a() {
            super(Cast.va);
        }

        public void c(int i, String str) {
            a(d(new Status(i, str, null)));
        }

        public void x(int i) {
            a(d(new Status(i)));
        }
    }

    private static abstract class b extends a<Status> {
        private b() {
        }

        public /* synthetic */ Result d(Status status) {
            return f(status);
        }

        public Status f(Status status) {
            return status;
        }
    }

    private static abstract class c extends a<ApplicationConnectionResult> {

        /* renamed from: com.google.android.gms.cast.Cast.c.1 */
        class AnonymousClass1 implements ApplicationConnectionResult {
            final /* synthetic */ Status vb;
            final /* synthetic */ c wB;

            AnonymousClass1(c cVar, Status status) {
                this.wB = cVar;
                this.vb = status;
            }

            public ApplicationMetadata getApplicationMetadata() {
                return null;
            }

            public String getApplicationStatus() {
                return null;
            }

            public String getSessionId() {
                return null;
            }

            public Status getStatus() {
                return this.vb;
            }

            public boolean getWasLaunched() {
                return false;
            }
        }

        private c() {
        }

        public /* synthetic */ Result d(Status status) {
            return h(status);
        }

        public ApplicationConnectionResult h(Status status) {
            return new AnonymousClass1(this, status);
        }
    }

    static {
        va = new com.google.android.gms.common.api.Api.b<dq>() {
            public /* synthetic */ com.google.android.gms.common.api.Api.a b(Context context, Looper looper, ee eeVar, ApiOptions apiOptions, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
                return c(context, looper, eeVar, apiOptions, connectionCallbacks, onConnectionFailedListener);
            }

            public dq c(Context context, Looper looper, ee eeVar, ApiOptions apiOptions, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
                er.b((Object) apiOptions, (Object) "Setting the API options is required.");
                er.b(apiOptions instanceof CastOptions, (Object) "Must provide valid CastOptions!");
                CastOptions castOptions = (CastOptions) apiOptions;
                return new dq(context, looper, castOptions.wv, (long) castOptions.wx, castOptions.ww, connectionCallbacks, onConnectionFailedListener);
            }

            public int getPriority() {
                return Integer.MAX_VALUE;
            }
        };
        API = new Api(va, new Scope[0]);
        CastApi = new a();
    }

    private Cast() {
    }
}
