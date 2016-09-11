package com.google.android.gms.drive.internal;

import android.os.RemoteException;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.drive.Drive;
import com.google.android.gms.drive.DriveApi.MetadataBufferResult;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.drive.DriveResource;
import com.google.android.gms.drive.DriveResource.MetadataResult;
import com.google.android.gms.drive.Metadata;
import com.google.android.gms.drive.MetadataBuffer;
import com.google.android.gms.drive.MetadataChangeSet;
import com.google.android.gms.drive.events.ChangeEvent;
import com.google.android.gms.drive.events.DriveEvent.Listener;

public class r implements DriveResource {
    protected final DriveId CS;

    private static class e implements MetadataResult {
        private final Metadata DQ;
        private final Status vl;

        public e(Status status, Metadata metadata) {
            this.vl = status;
            this.DQ = metadata;
        }

        public Metadata getMetadata() {
            return this.DQ;
        }

        public Status getStatus() {
            return this.vl;
        }
    }

    private abstract class a extends m<MetadataResult> {
        final /* synthetic */ r DP;

        private a(r rVar) {
            this.DP = rVar;
        }

        public /* synthetic */ Result d(Status status) {
            return r(status);
        }

        public MetadataResult r(Status status) {
            return new e(status, null);
        }
    }

    private static class b extends c {
        private final com.google.android.gms.common.api.a.c<MetadataBufferResult> vj;

        public b(com.google.android.gms.common.api.a.c<MetadataBufferResult> cVar) {
            this.vj = cVar;
        }

        public void a(OnListParentsResponse onListParentsResponse) throws RemoteException {
            this.vj.b(new e(Status.zQ, new MetadataBuffer(onListParentsResponse.fd(), null)));
        }

        public void l(Status status) throws RemoteException {
            this.vj.b(new e(status, null));
        }
    }

    private abstract class c extends m<MetadataBufferResult> {
        final /* synthetic */ r DP;

        private c(r rVar) {
            this.DP = rVar;
        }

        public /* synthetic */ Result d(Status status) {
            return o(status);
        }

        public MetadataBufferResult o(Status status) {
            return new e(status, null);
        }
    }

    private static class d extends c {
        private final com.google.android.gms.common.api.a.c<MetadataResult> vj;

        public d(com.google.android.gms.common.api.a.c<MetadataResult> cVar) {
            this.vj = cVar;
        }

        public void a(OnMetadataResponse onMetadataResponse) throws RemoteException {
            this.vj.b(new e(Status.zQ, new j(onMetadataResponse.fe())));
        }

        public void l(Status status) throws RemoteException {
            this.vj.b(new e(status, null));
        }
    }

    private abstract class f extends m<MetadataResult> {
        final /* synthetic */ r DP;

        private f(r rVar) {
            this.DP = rVar;
        }

        public /* synthetic */ Result d(Status status) {
            return r(status);
        }

        public MetadataResult r(Status status) {
            return new e(status, null);
        }
    }

    /* renamed from: com.google.android.gms.drive.internal.r.3 */
    class AnonymousClass3 extends f {
        final /* synthetic */ MetadataChangeSet DK;
        final /* synthetic */ r DP;

        AnonymousClass3(r rVar, MetadataChangeSet metadataChangeSet) {
            this.DP = rVar;
            this.DK = metadataChangeSet;
            super(null);
        }

        protected void a(n nVar) throws RemoteException {
            nVar.eT().a(new UpdateMetadataRequest(this.DP.CS, this.DK.eS()), new d(this));
        }
    }

    protected r(DriveId driveId) {
        this.CS = driveId;
    }

    public PendingResult<Status> addChangeListener(GoogleApiClient apiClient, Listener<ChangeEvent> listener) {
        return ((n) apiClient.a(Drive.va)).a(apiClient, this.CS, 1, listener);
    }

    public DriveId getDriveId() {
        return this.CS;
    }

    public PendingResult<MetadataResult> getMetadata(GoogleApiClient apiClient) {
        return apiClient.a(new a() {
            final /* synthetic */ r DP;

            {
                this.DP = r2;
            }

            protected void a(n nVar) throws RemoteException {
                nVar.eT().a(new GetMetadataRequest(this.DP.CS), new d(this));
            }
        });
    }

    public PendingResult<MetadataBufferResult> listParents(GoogleApiClient apiClient) {
        return apiClient.a(new c() {
            final /* synthetic */ r DP;

            {
                this.DP = r2;
            }

            protected void a(n nVar) throws RemoteException {
                nVar.eT().a(new ListParentsRequest(this.DP.CS), new b(this));
            }
        });
    }

    public PendingResult<Status> removeChangeListener(GoogleApiClient apiClient, Listener<ChangeEvent> listener) {
        return ((n) apiClient.a(Drive.va)).b(apiClient, this.CS, 1, listener);
    }

    public PendingResult<MetadataResult> updateMetadata(GoogleApiClient apiClient, MetadataChangeSet changeSet) {
        if (changeSet != null) {
            return apiClient.b(new AnonymousClass3(this, changeSet));
        }
        throw new IllegalArgumentException("ChangeSet must be provided.");
    }
}
