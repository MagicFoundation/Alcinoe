package com.google.android.gms.internal;

import android.content.Intent;
import android.graphics.Bitmap;
import android.os.Bundle;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.common.api.a.c;
import com.google.android.gms.games.Games;
import com.google.android.gms.games.request.GameRequest;
import com.google.android.gms.games.request.GameRequestBuffer;
import com.google.android.gms.games.request.OnRequestReceivedListener;
import com.google.android.gms.games.request.Requests;
import com.google.android.gms.games.request.Requests.LoadRequestsResult;
import com.google.android.gms.games.request.Requests.UpdateRequestsResult;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public final class go implements Requests {

    private static abstract class a extends com.google.android.gms.games.Games.a<LoadRequestsResult> {

        /* renamed from: com.google.android.gms.internal.go.a.1 */
        class AnonymousClass1 implements LoadRequestsResult {
            final /* synthetic */ a In;
            final /* synthetic */ Status vb;

            AnonymousClass1(a aVar, Status status) {
                this.In = aVar;
                this.vb = status;
            }

            public GameRequestBuffer getRequests(int type) {
                return null;
            }

            public Status getStatus() {
                return this.vb;
            }

            public void release() {
            }
        }

        private a() {
        }

        public LoadRequestsResult B(Status status) {
            return new AnonymousClass1(this, status);
        }

        public /* synthetic */ Result d(Status status) {
            return B(status);
        }
    }

    private static abstract class b extends com.google.android.gms.games.Games.a<UpdateRequestsResult> {

        /* renamed from: com.google.android.gms.internal.go.b.1 */
        class AnonymousClass1 implements UpdateRequestsResult {
            final /* synthetic */ b Io;
            final /* synthetic */ Status vb;

            AnonymousClass1(b bVar, Status status) {
                this.Io = bVar;
                this.vb = status;
            }

            public Set<String> getRequestIds() {
                return null;
            }

            public int getRequestOutcome(String requestId) {
                throw new IllegalArgumentException("Unknown request ID " + requestId);
            }

            public Status getStatus() {
                return this.vb;
            }

            public void release() {
            }
        }

        private b() {
        }

        public UpdateRequestsResult C(Status status) {
            return new AnonymousClass1(this, status);
        }

        public /* synthetic */ Result d(Status status) {
            return C(status);
        }
    }

    /* renamed from: com.google.android.gms.internal.go.1 */
    class AnonymousClass1 extends b {
        final /* synthetic */ String[] Ij;
        final /* synthetic */ go Ik;

        AnonymousClass1(go goVar, String[] strArr) {
            this.Ik = goVar;
            this.Ij = strArr;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((c) this, this.Ij);
        }
    }

    /* renamed from: com.google.android.gms.internal.go.2 */
    class AnonymousClass2 extends b {
        final /* synthetic */ String[] Ij;
        final /* synthetic */ go Ik;

        AnonymousClass2(go goVar, String[] strArr) {
            this.Ik = goVar;
            this.Ij = strArr;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.b((c) this, this.Ij);
        }
    }

    /* renamed from: com.google.android.gms.internal.go.3 */
    class AnonymousClass3 extends a {
        final /* synthetic */ int HP;
        final /* synthetic */ go Ik;
        final /* synthetic */ int Il;
        final /* synthetic */ int Im;

        AnonymousClass3(go goVar, int i, int i2, int i3) {
            this.Ik = goVar;
            this.Il = i;
            this.Im = i2;
            this.HP = i3;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((c) this, this.Il, this.Im, this.HP);
        }
    }

    public PendingResult<UpdateRequestsResult> acceptRequest(GoogleApiClient apiClient, String requestId) {
        List arrayList = new ArrayList();
        arrayList.add(requestId);
        return acceptRequests(apiClient, arrayList);
    }

    public PendingResult<UpdateRequestsResult> acceptRequests(GoogleApiClient apiClient, List<String> requestIds) {
        return apiClient.b(new AnonymousClass1(this, requestIds == null ? null : (String[]) requestIds.toArray(new String[requestIds.size()])));
    }

    public PendingResult<UpdateRequestsResult> dismissRequest(GoogleApiClient apiClient, String requestId) {
        List arrayList = new ArrayList();
        arrayList.add(requestId);
        return dismissRequests(apiClient, arrayList);
    }

    public PendingResult<UpdateRequestsResult> dismissRequests(GoogleApiClient apiClient, List<String> requestIds) {
        return apiClient.b(new AnonymousClass2(this, requestIds == null ? null : (String[]) requestIds.toArray(new String[requestIds.size()])));
    }

    public ArrayList<GameRequest> getGameRequestsFromBundle(Bundle extras) {
        if (extras == null || !extras.containsKey(Requests.EXTRA_REQUESTS)) {
            return new ArrayList();
        }
        ArrayList arrayList = (ArrayList) extras.get(Requests.EXTRA_REQUESTS);
        ArrayList<GameRequest> arrayList2 = new ArrayList();
        int size = arrayList.size();
        for (int i = 0; i < size; i++) {
            arrayList2.add((GameRequest) arrayList.get(i));
        }
        return arrayList2;
    }

    public ArrayList<GameRequest> getGameRequestsFromInboxResponse(Intent response) {
        return response == null ? new ArrayList() : getGameRequestsFromBundle(response.getExtras());
    }

    public Intent getInboxIntent(GoogleApiClient apiClient) {
        return Games.c(apiClient).fD();
    }

    public int getMaxLifetimeDays(GoogleApiClient apiClient) {
        return Games.c(apiClient).fF();
    }

    public int getMaxPayloadSize(GoogleApiClient apiClient) {
        return Games.c(apiClient).fE();
    }

    public Intent getSendIntent(GoogleApiClient apiClient, int type, byte[] payload, int requestLifetimeDays, Bitmap icon, String description) {
        return Games.c(apiClient).a(type, payload, requestLifetimeDays, icon, description);
    }

    public PendingResult<LoadRequestsResult> loadRequests(GoogleApiClient apiClient, int requestDirection, int types, int sortOrder) {
        return apiClient.a(new AnonymousClass3(this, requestDirection, types, sortOrder));
    }

    public void registerRequestListener(GoogleApiClient apiClient, OnRequestReceivedListener listener) {
        Games.c(apiClient).a(listener);
    }

    public void unregisterRequestListener(GoogleApiClient apiClient) {
        Games.c(apiClient).fx();
    }
}
