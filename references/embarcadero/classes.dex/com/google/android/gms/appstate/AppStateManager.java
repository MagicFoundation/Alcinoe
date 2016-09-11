package com.google.android.gms.appstate;

import android.content.Context;
import android.os.Looper;
import com.google.android.gms.common.Scopes;
import com.google.android.gms.common.api.Api;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.GoogleApiClient.ApiOptions;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Releasable;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Scope;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.internal.dl;
import com.google.android.gms.internal.ee;
import com.google.android.gms.internal.er;

public final class AppStateManager {
    public static final Api API;
    public static final Scope SCOPE_APP_STATE;
    static final com.google.android.gms.common.api.Api.b<dl> va;

    public interface StateConflictResult extends Releasable, Result {
        byte[] getLocalData();

        String getResolvedVersion();

        byte[] getServerData();

        int getStateKey();
    }

    public interface StateDeletedResult extends Result {
        int getStateKey();
    }

    public interface StateListResult extends Result {
        AppStateBuffer getStateBuffer();
    }

    public interface StateLoadedResult extends Releasable, Result {
        byte[] getLocalData();

        int getStateKey();
    }

    public interface StateResult extends Releasable, Result {
        StateConflictResult getConflictResult();

        StateLoadedResult getLoadedResult();
    }

    /* renamed from: com.google.android.gms.appstate.AppStateManager.2 */
    static class AnonymousClass2 implements StateResult {
        final /* synthetic */ Status vb;

        AnonymousClass2(Status status) {
            this.vb = status;
        }

        public StateConflictResult getConflictResult() {
            return null;
        }

        public StateLoadedResult getLoadedResult() {
            return null;
        }

        public Status getStatus() {
            return this.vb;
        }

        public void release() {
        }
    }

    public static abstract class a<R extends Result> extends com.google.android.gms.common.api.a.a<R, dl> implements PendingResult<R> {
        public a() {
            super(AppStateManager.va);
        }
    }

    private static abstract class b extends a<StateDeletedResult> {
        private b() {
        }
    }

    private static abstract class c extends a<StateListResult> {

        /* renamed from: com.google.android.gms.appstate.AppStateManager.c.1 */
        class AnonymousClass1 implements StateListResult {
            final /* synthetic */ Status vb;
            final /* synthetic */ c vh;

            AnonymousClass1(c cVar, Status status) {
                this.vh = cVar;
                this.vb = status;
            }

            public AppStateBuffer getStateBuffer() {
                return new AppStateBuffer(null);
            }

            public Status getStatus() {
                return this.vb;
            }
        }

        private c() {
        }

        public /* synthetic */ Result d(Status status) {
            return e(status);
        }

        public StateListResult e(Status status) {
            return new AnonymousClass1(this, status);
        }
    }

    private static abstract class d extends a<Status> {
        private d() {
        }

        public /* synthetic */ Result d(Status status) {
            return f(status);
        }

        public Status f(Status status) {
            return status;
        }
    }

    private static abstract class e extends a<StateResult> {
        private e() {
        }

        public /* synthetic */ Result d(Status status) {
            return g(status);
        }

        public StateResult g(Status status) {
            return AppStateManager.a(status);
        }
    }

    /* renamed from: com.google.android.gms.appstate.AppStateManager.3 */
    static class AnonymousClass3 extends e {
        final /* synthetic */ int vc;
        final /* synthetic */ byte[] vd;

        AnonymousClass3(int i, byte[] bArr) {
            this.vc = i;
            this.vd = bArr;
            super();
        }

        protected void a(dl dlVar) {
            dlVar.a(null, this.vc, this.vd);
        }
    }

    /* renamed from: com.google.android.gms.appstate.AppStateManager.4 */
    static class AnonymousClass4 extends e {
        final /* synthetic */ int vc;
        final /* synthetic */ byte[] vd;

        AnonymousClass4(int i, byte[] bArr) {
            this.vc = i;
            this.vd = bArr;
            super();
        }

        protected void a(dl dlVar) {
            dlVar.a(this, this.vc, this.vd);
        }
    }

    /* renamed from: com.google.android.gms.appstate.AppStateManager.5 */
    static class AnonymousClass5 extends b {
        final /* synthetic */ int vc;

        /* renamed from: com.google.android.gms.appstate.AppStateManager.5.1 */
        class AnonymousClass1 implements StateDeletedResult {
            final /* synthetic */ Status vb;
            final /* synthetic */ AnonymousClass5 ve;

            AnonymousClass1(AnonymousClass5 anonymousClass5, Status status) {
                this.ve = anonymousClass5;
                this.vb = status;
            }

            public int getStateKey() {
                return this.ve.vc;
            }

            public Status getStatus() {
                return this.vb;
            }
        }

        AnonymousClass5(int i) {
            this.vc = i;
            super();
        }

        protected void a(dl dlVar) {
            dlVar.a((com.google.android.gms.common.api.a.c) this, this.vc);
        }

        public StateDeletedResult c(Status status) {
            return new AnonymousClass1(this, status);
        }

        public /* synthetic */ Result d(Status status) {
            return c(status);
        }
    }

    /* renamed from: com.google.android.gms.appstate.AppStateManager.6 */
    static class AnonymousClass6 extends e {
        final /* synthetic */ int vc;

        AnonymousClass6(int i) {
            this.vc = i;
            super();
        }

        protected void a(dl dlVar) {
            dlVar.b(this, this.vc);
        }
    }

    /* renamed from: com.google.android.gms.appstate.AppStateManager.8 */
    static class AnonymousClass8 extends e {
        final /* synthetic */ int vc;
        final /* synthetic */ String vf;
        final /* synthetic */ byte[] vg;

        AnonymousClass8(int i, String str, byte[] bArr) {
            this.vc = i;
            this.vf = str;
            this.vg = bArr;
            super();
        }

        protected void a(dl dlVar) {
            dlVar.a(this, this.vc, this.vf, this.vg);
        }
    }

    static {
        va = new com.google.android.gms.common.api.Api.b<dl>() {
            public dl a(Context context, Looper looper, ee eeVar, ApiOptions apiOptions, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
                return new dl(context, looper, connectionCallbacks, onConnectionFailedListener, eeVar.dR(), (String[]) eeVar.dT().toArray(new String[0]));
            }

            public /* synthetic */ com.google.android.gms.common.api.Api.a b(Context context, Looper looper, ee eeVar, ApiOptions apiOptions, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
                return a(context, looper, eeVar, apiOptions, connectionCallbacks, onConnectionFailedListener);
            }

            public int getPriority() {
                return Integer.MAX_VALUE;
            }
        };
        SCOPE_APP_STATE = new Scope(Scopes.APP_STATE);
        API = new Api(va, SCOPE_APP_STATE);
    }

    private AppStateManager() {
    }

    private static StateResult a(Status status) {
        return new AnonymousClass2(status);
    }

    public static dl a(GoogleApiClient googleApiClient) {
        boolean z = true;
        er.b(googleApiClient != null, (Object) "GoogleApiClient parameter is required.");
        er.a(googleApiClient.isConnected(), "GoogleApiClient must be connected.");
        dl dlVar = (dl) googleApiClient.a(va);
        if (dlVar == null) {
            z = false;
        }
        er.a(z, "GoogleApiClient is not configured to use the AppState API. Pass AppStateManager.API into GoogleApiClient.Builder#addApi() to use this feature.");
        return dlVar;
    }

    public static PendingResult<StateDeletedResult> delete(GoogleApiClient googleApiClient, int stateKey) {
        return googleApiClient.b(new AnonymousClass5(stateKey));
    }

    public static int getMaxNumKeys(GoogleApiClient googleApiClient) {
        return a(googleApiClient).cO();
    }

    public static int getMaxStateSize(GoogleApiClient googleApiClient) {
        return a(googleApiClient).cN();
    }

    public static PendingResult<StateListResult> list(GoogleApiClient googleApiClient) {
        return googleApiClient.a(new c() {
            protected void a(dl dlVar) {
                dlVar.a(this);
            }
        });
    }

    public static PendingResult<StateResult> load(GoogleApiClient googleApiClient, int stateKey) {
        return googleApiClient.a(new AnonymousClass6(stateKey));
    }

    public static PendingResult<StateResult> resolve(GoogleApiClient googleApiClient, int stateKey, String resolvedVersion, byte[] resolvedData) {
        return googleApiClient.b(new AnonymousClass8(stateKey, resolvedVersion, resolvedData));
    }

    public static PendingResult<Status> signOut(GoogleApiClient googleApiClient) {
        return googleApiClient.b(new d() {
            protected void a(dl dlVar) {
                dlVar.b((com.google.android.gms.common.api.a.c) this);
            }
        });
    }

    public static void update(GoogleApiClient googleApiClient, int stateKey, byte[] data) {
        googleApiClient.b(new AnonymousClass3(stateKey, data));
    }

    public static PendingResult<StateResult> updateImmediate(GoogleApiClient googleApiClient, int stateKey, byte[] data) {
        return googleApiClient.b(new AnonymousClass4(stateKey, data));
    }
}
