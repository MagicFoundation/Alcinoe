package com.google.android.gms.internal;

import com.google.android.gms.common.api.Api.b;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.plus.Account;
import com.google.android.gms.plus.internal.e;

public final class ii implements Account {
    private final b<e> Rw;

    private static abstract class a extends com.google.android.gms.plus.Plus.a<Status> {
        a(b<e> bVar) {
            super(bVar);
        }

        public /* synthetic */ Result d(Status status) {
            return f(status);
        }

        public Status f(Status status) {
            return status;
        }
    }

    /* renamed from: com.google.android.gms.internal.ii.1 */
    class AnonymousClass1 extends a {
        final /* synthetic */ ii Rx;

        AnonymousClass1(ii iiVar, b bVar) {
            this.Rx = iiVar;
            super(bVar);
        }

        protected void a(e eVar) {
            eVar.k(this);
        }
    }

    public ii(b<e> bVar) {
        this.Rw = bVar;
    }

    private static e a(GoogleApiClient googleApiClient, b<e> bVar) {
        boolean z = true;
        er.b(googleApiClient != null, (Object) "GoogleApiClient parameter is required.");
        er.a(googleApiClient.isConnected(), "GoogleApiClient must be connected.");
        e eVar = (e) googleApiClient.a((b) bVar);
        if (eVar == null) {
            z = false;
        }
        er.a(z, "GoogleApiClient is not configured to use the Plus.API Api. Pass this into GoogleApiClient.Builder#addApi() to use this feature.");
        return eVar;
    }

    public void clearDefaultAccount(GoogleApiClient googleApiClient) {
        a(googleApiClient, this.Rw).clearDefaultAccount();
    }

    public String getAccountName(GoogleApiClient googleApiClient) {
        return a(googleApiClient, this.Rw).getAccountName();
    }

    public PendingResult<Status> revokeAccessAndDisconnect(GoogleApiClient googleApiClient) {
        return googleApiClient.b(new AnonymousClass1(this, this.Rw));
    }
}
