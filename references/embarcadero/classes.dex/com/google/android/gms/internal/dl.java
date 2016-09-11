package com.google.android.gms.internal;

import android.content.Context;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Looper;
import android.os.RemoteException;
import android.util.Log;
import com.google.android.gms.appstate.AppStateBuffer;
import com.google.android.gms.appstate.AppStateManager.StateConflictResult;
import com.google.android.gms.appstate.AppStateManager.StateDeletedResult;
import com.google.android.gms.appstate.AppStateManager.StateListResult;
import com.google.android.gms.appstate.AppStateManager.StateLoadedResult;
import com.google.android.gms.appstate.AppStateManager.StateResult;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.common.Scopes;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.games.GamesStatusCodes;

public final class dl extends eh<dn> {
    private final String vi;

    final class h extends b<com.google.android.gms.common.api.a.c<Status>> {
        final /* synthetic */ dl vk;
        private final Status vl;

        public h(dl dlVar, com.google.android.gms.common.api.a.c<Status> cVar, Status status) {
            this.vk = dlVar;
            super(dlVar, cVar);
            this.vl = status;
        }

        public /* synthetic */ void a(Object obj) {
            c((com.google.android.gms.common.api.a.c) obj);
        }

        public void c(com.google.android.gms.common.api.a.c<Status> cVar) {
            cVar.b(this.vl);
        }

        protected void cP() {
        }
    }

    final class b extends b<com.google.android.gms.common.api.a.c<StateDeletedResult>> implements StateDeletedResult {
        final /* synthetic */ dl vk;
        private final Status vl;
        private final int vm;

        public b(dl dlVar, com.google.android.gms.common.api.a.c<StateDeletedResult> cVar, Status status, int i) {
            this.vk = dlVar;
            super(dlVar, cVar);
            this.vl = status;
            this.vm = i;
        }

        public /* synthetic */ void a(Object obj) {
            c((com.google.android.gms.common.api.a.c) obj);
        }

        public void c(com.google.android.gms.common.api.a.c<StateDeletedResult> cVar) {
            cVar.b(this);
        }

        protected void cP() {
        }

        public int getStateKey() {
            return this.vm;
        }

        public Status getStatus() {
            return this.vl;
        }
    }

    final class d extends com.google.android.gms.internal.eh.d<com.google.android.gms.common.api.a.c<StateListResult>> implements StateListResult {
        final /* synthetic */ dl vk;
        private final Status vl;
        private final AppStateBuffer vn;

        public d(dl dlVar, com.google.android.gms.common.api.a.c<StateListResult> cVar, Status status, DataHolder dataHolder) {
            this.vk = dlVar;
            super(dlVar, cVar, dataHolder);
            this.vl = status;
            this.vn = new AppStateBuffer(dataHolder);
        }

        public void a(com.google.android.gms.common.api.a.c<StateListResult> cVar, DataHolder dataHolder) {
            cVar.b(this);
        }

        public AppStateBuffer getStateBuffer() {
            return this.vn;
        }

        public Status getStatus() {
            return this.vl;
        }
    }

    final class f extends com.google.android.gms.internal.eh.d<com.google.android.gms.common.api.a.c<StateResult>> implements StateConflictResult, StateLoadedResult, StateResult {
        final /* synthetic */ dl vk;
        private final Status vl;
        private final int vm;
        private final AppStateBuffer vn;

        public f(dl dlVar, com.google.android.gms.common.api.a.c<StateResult> cVar, int i, DataHolder dataHolder) {
            this.vk = dlVar;
            super(dlVar, cVar, dataHolder);
            this.vm = i;
            this.vl = new Status(dataHolder.getStatusCode());
            this.vn = new AppStateBuffer(dataHolder);
        }

        private boolean cQ() {
            return this.vl.getStatusCode() == GamesStatusCodes.STATUS_REQUEST_UPDATE_PARTIAL_SUCCESS;
        }

        public void a(com.google.android.gms.common.api.a.c<StateResult> cVar, DataHolder dataHolder) {
            cVar.b(this);
        }

        public StateConflictResult getConflictResult() {
            return cQ() ? this : null;
        }

        public StateLoadedResult getLoadedResult() {
            return cQ() ? null : this;
        }

        public byte[] getLocalData() {
            return this.vn.getCount() == 0 ? null : this.vn.get(0).getLocalData();
        }

        public String getResolvedVersion() {
            return this.vn.getCount() == 0 ? null : this.vn.get(0).getConflictVersion();
        }

        public byte[] getServerData() {
            return this.vn.getCount() == 0 ? null : this.vn.get(0).getConflictData();
        }

        public int getStateKey() {
            return this.vm;
        }

        public Status getStatus() {
            return this.vl;
        }

        public void release() {
            this.vn.close();
        }
    }

    final class a extends dk {
        private final com.google.android.gms.common.api.a.c<StateDeletedResult> vj;
        final /* synthetic */ dl vk;

        public a(dl dlVar, com.google.android.gms.common.api.a.c<StateDeletedResult> cVar) {
            this.vk = dlVar;
            this.vj = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Result holder must not be null");
        }

        public void b(int i, int i2) {
            this.vk.a(new b(this.vk, this.vj, new Status(i), i2));
        }
    }

    final class c extends dk {
        private final com.google.android.gms.common.api.a.c<StateListResult> vj;
        final /* synthetic */ dl vk;

        public c(dl dlVar, com.google.android.gms.common.api.a.c<StateListResult> cVar) {
            this.vk = dlVar;
            this.vj = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Result holder must not be null");
        }

        public void a(DataHolder dataHolder) {
            this.vk.a(new d(this.vk, this.vj, new Status(dataHolder.getStatusCode()), dataHolder));
        }
    }

    final class e extends dk {
        private final com.google.android.gms.common.api.a.c<StateResult> vj;
        final /* synthetic */ dl vk;

        public e(dl dlVar, com.google.android.gms.common.api.a.c<StateResult> cVar) {
            this.vk = dlVar;
            this.vj = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Result holder must not be null");
        }

        public void a(int i, DataHolder dataHolder) {
            this.vk.a(new f(this.vk, this.vj, i, dataHolder));
        }
    }

    final class g extends dk {
        com.google.android.gms.common.api.a.c<Status> vj;
        final /* synthetic */ dl vk;

        public g(dl dlVar, com.google.android.gms.common.api.a.c<Status> cVar) {
            this.vk = dlVar;
            this.vj = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void cM() {
            this.vk.a(new h(this.vk, this.vj, new Status(0)));
        }
    }

    public dl(Context context, Looper looper, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener, String str, String[] strArr) {
        super(context, looper, connectionCallbacks, onConnectionFailedListener, strArr);
        this.vi = (String) er.f(str);
    }

    public void a(com.google.android.gms.common.api.a.c<StateListResult> cVar) {
        try {
            ((dn) eb()).a(new c(this, cVar));
        } catch (RemoteException e) {
            Log.w("AppStateClient", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<StateDeletedResult> cVar, int i) {
        try {
            ((dn) eb()).b(new a(this, cVar), i);
        } catch (RemoteException e) {
            Log.w("AppStateClient", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<StateResult> cVar, int i, String str, byte[] bArr) {
        try {
            ((dn) eb()).a(new e(this, cVar), i, str, bArr);
        } catch (RemoteException e) {
            Log.w("AppStateClient", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<StateResult> cVar, int i, byte[] bArr) {
        if (cVar == null) {
            dm dmVar = null;
        } else {
            Object eVar = new e(this, cVar);
        }
        try {
            ((dn) eb()).a(dmVar, i, bArr);
        } catch (RemoteException e) {
            Log.w("AppStateClient", "service died");
        }
    }

    protected void a(en enVar, com.google.android.gms.internal.eh.e eVar) throws RemoteException {
        enVar.a((em) eVar, (int) GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_VERSION_CODE, getContext().getPackageName(), this.vi, ea());
    }

    protected String aF() {
        return "com.google.android.gms.appstate.service.START";
    }

    protected String aG() {
        return "com.google.android.gms.appstate.internal.IAppStateService";
    }

    public void b(com.google.android.gms.common.api.a.c<Status> cVar) {
        try {
            ((dn) eb()).b(new g(this, cVar));
        } catch (RemoteException e) {
            Log.w("AppStateClient", "service died");
        }
    }

    public void b(com.google.android.gms.common.api.a.c<StateResult> cVar, int i) {
        try {
            ((dn) eb()).a(new e(this, cVar), i);
        } catch (RemoteException e) {
            Log.w("AppStateClient", "service died");
        }
    }

    protected void b(String... strArr) {
        boolean z = false;
        for (String equals : strArr) {
            if (equals.equals(Scopes.APP_STATE)) {
                z = true;
            }
        }
        er.a(z, String.format("App State APIs requires %s to function.", new Object[]{Scopes.APP_STATE}));
    }

    public int cN() {
        try {
            return ((dn) eb()).cN();
        } catch (RemoteException e) {
            Log.w("AppStateClient", "service died");
            return 2;
        }
    }

    public int cO() {
        try {
            return ((dn) eb()).cO();
        } catch (RemoteException e) {
            Log.w("AppStateClient", "service died");
            return 2;
        }
    }

    protected /* synthetic */ IInterface p(IBinder iBinder) {
        return s(iBinder);
    }

    protected dn s(IBinder iBinder) {
        return com.google.android.gms.internal.dn.a.u(iBinder);
    }
}
