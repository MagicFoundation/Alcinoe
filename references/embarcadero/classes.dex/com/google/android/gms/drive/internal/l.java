package com.google.android.gms.drive.internal;

import android.os.RemoteException;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.drive.Contents;
import com.google.android.gms.drive.CreateFileActivityBuilder;
import com.google.android.gms.drive.Drive;
import com.google.android.gms.drive.DriveApi;
import com.google.android.gms.drive.DriveApi.ContentsResult;
import com.google.android.gms.drive.DriveApi.DriveIdResult;
import com.google.android.gms.drive.DriveApi.MetadataBufferResult;
import com.google.android.gms.drive.DriveFile;
import com.google.android.gms.drive.DriveFolder;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.drive.MetadataBuffer;
import com.google.android.gms.drive.OpenFileActivityBuilder;
import com.google.android.gms.drive.query.Query;

public class l implements DriveApi {

    static class a implements ContentsResult {
        private final Contents CW;
        private final Status vl;

        public a(Status status, Contents contents) {
            this.vl = status;
            this.CW = contents;
        }

        public Contents getContents() {
            return this.CW;
        }

        public Status getStatus() {
            return this.vl;
        }
    }

    static class c implements DriveIdResult {
        private final DriveId CS;
        private final Status vl;

        public c(Status status, DriveId driveId) {
            this.vl = status;
            this.CS = driveId;
        }

        public DriveId getDriveId() {
            return this.CS;
        }

        public Status getStatus() {
            return this.vl;
        }
    }

    static class e implements MetadataBufferResult {
        private final MetadataBuffer Dy;
        private final Status vl;

        public e(Status status, MetadataBuffer metadataBuffer) {
            this.vl = status;
            this.Dy = metadataBuffer;
        }

        public MetadataBuffer getMetadataBuffer() {
            return this.Dy;
        }

        public Status getStatus() {
            return this.vl;
        }
    }

    private static class b extends c {
        private final com.google.android.gms.common.api.a.c<DriveIdResult> vj;

        public b(com.google.android.gms.common.api.a.c<DriveIdResult> cVar) {
            this.vj = cVar;
        }

        public void a(OnMetadataResponse onMetadataResponse) throws RemoteException {
            this.vj.b(new c(Status.zQ, new j(onMetadataResponse.fe()).getDriveId()));
        }

        public void l(Status status) throws RemoteException {
            this.vj.b(new c(status, null));
        }
    }

    abstract class d extends m<DriveIdResult> {
        final /* synthetic */ l Dv;

        d(l lVar) {
            this.Dv = lVar;
        }

        public /* synthetic */ Result d(Status status) {
            return m(status);
        }

        public DriveIdResult m(Status status) {
            return new c(status, null);
        }
    }

    private static class f extends c {
        private final com.google.android.gms.common.api.a.c<ContentsResult> vj;

        public f(com.google.android.gms.common.api.a.c<ContentsResult> cVar) {
            this.vj = cVar;
        }

        public void a(OnContentsResponse onContentsResponse) throws RemoteException {
            this.vj.b(new a(Status.zQ, onContentsResponse.eX()));
        }

        public void l(Status status) throws RemoteException {
            this.vj.b(new a(status, null));
        }
    }

    abstract class g extends m<ContentsResult> {
        final /* synthetic */ l Dv;

        g(l lVar) {
            this.Dv = lVar;
        }

        public /* synthetic */ Result d(Status status) {
            return n(status);
        }

        public ContentsResult n(Status status) {
            return new a(status, null);
        }
    }

    static class h extends c {
        private final com.google.android.gms.common.api.a.c<MetadataBufferResult> vj;

        public h(com.google.android.gms.common.api.a.c<MetadataBufferResult> cVar) {
            this.vj = cVar;
        }

        public void a(OnListEntriesResponse onListEntriesResponse) throws RemoteException {
            this.vj.b(new e(Status.zQ, new MetadataBuffer(onListEntriesResponse.fc(), null)));
        }

        public void l(Status status) throws RemoteException {
            this.vj.b(new e(status, null));
        }
    }

    abstract class i extends m<MetadataBufferResult> {
        final /* synthetic */ l Dv;

        i(l lVar) {
            this.Dv = lVar;
        }

        public /* synthetic */ Result d(Status status) {
            return o(status);
        }

        public MetadataBufferResult o(Status status) {
            return new e(status, null);
        }
    }

    static abstract class j extends m<Status> {
        j() {
        }

        public /* synthetic */ Result d(Status status) {
            return f(status);
        }

        public Status f(Status status) {
            return status;
        }
    }

    abstract class l extends m<Status> {
        final /* synthetic */ l Dv;

        l(l lVar) {
            this.Dv = lVar;
        }

        public /* synthetic */ Result d(Status status) {
            return f(status);
        }

        public Status f(Status status) {
            return status;
        }
    }

    /* renamed from: com.google.android.gms.drive.internal.l.1 */
    class AnonymousClass1 extends i {
        final /* synthetic */ Query Du;
        final /* synthetic */ l Dv;

        AnonymousClass1(l lVar, Query query) {
            this.Dv = lVar;
            this.Du = query;
            super(lVar);
        }

        protected void a(n nVar) throws RemoteException {
            nVar.eT().a(new QueryRequest(this.Du), new h(this));
        }
    }

    /* renamed from: com.google.android.gms.drive.internal.l.3 */
    class AnonymousClass3 extends j {
        final /* synthetic */ l Dv;
        final /* synthetic */ Contents Dw;

        AnonymousClass3(l lVar, Contents contents) {
            this.Dv = lVar;
            this.Dw = contents;
        }

        protected void a(n nVar) throws RemoteException {
            nVar.eT().a(new CloseContentsRequest(this.Dw, false), new ak(this));
        }
    }

    /* renamed from: com.google.android.gms.drive.internal.l.4 */
    class AnonymousClass4 extends d {
        final /* synthetic */ l Dv;
        final /* synthetic */ String Dx;

        AnonymousClass4(l lVar, String str) {
            this.Dv = lVar;
            this.Dx = str;
            super(lVar);
        }

        protected void a(n nVar) throws RemoteException {
            nVar.eT().a(new GetMetadataRequest(DriveId.aq(this.Dx)), new b(this));
        }
    }

    static class k extends j {
        k(Status status) {
            a((Result) status);
        }

        protected void a(n nVar) {
        }
    }

    public PendingResult<Status> discardContents(GoogleApiClient apiClient, Contents contents) {
        return apiClient.b(new AnonymousClass3(this, contents));
    }

    public PendingResult<DriveIdResult> fetchDriveId(GoogleApiClient apiClient, String resourceId) {
        return apiClient.a(new AnonymousClass4(this, resourceId));
    }

    public DriveFolder getAppFolder(GoogleApiClient apiClient) {
        if (apiClient.isConnected()) {
            DriveId eV = ((n) apiClient.a(Drive.va)).eV();
            return eV != null ? new q(eV) : null;
        } else {
            throw new IllegalStateException("Client must be connected");
        }
    }

    public DriveFile getFile(GoogleApiClient apiClient, DriveId id) {
        if (id == null) {
            throw new IllegalArgumentException("Id must be provided.");
        } else if (apiClient.isConnected()) {
            return new o(id);
        } else {
            throw new IllegalStateException("Client must be connected");
        }
    }

    public DriveFolder getFolder(GoogleApiClient apiClient, DriveId id) {
        if (id == null) {
            throw new IllegalArgumentException("Id must be provided.");
        } else if (apiClient.isConnected()) {
            return new q(id);
        } else {
            throw new IllegalStateException("Client must be connected");
        }
    }

    public DriveFolder getRootFolder(GoogleApiClient apiClient) {
        if (apiClient.isConnected()) {
            return new q(((n) apiClient.a(Drive.va)).eU());
        }
        throw new IllegalStateException("Client must be connected");
    }

    public PendingResult<ContentsResult> newContents(GoogleApiClient apiClient) {
        return apiClient.a(new g() {
            final /* synthetic */ l Dv;

            {
                this.Dv = r1;
            }

            protected void a(n nVar) throws RemoteException {
                nVar.eT().a(new CreateContentsRequest(), new f(this));
            }
        });
    }

    public CreateFileActivityBuilder newCreateFileActivityBuilder() {
        return new CreateFileActivityBuilder();
    }

    public OpenFileActivityBuilder newOpenFileActivityBuilder() {
        return new OpenFileActivityBuilder();
    }

    public PendingResult<MetadataBufferResult> query(GoogleApiClient apiClient, Query query) {
        if (query != null) {
            return apiClient.a(new AnonymousClass1(this, query));
        }
        throw new IllegalArgumentException("Query must be provided.");
    }

    public PendingResult<Status> requestSync(GoogleApiClient client) {
        return client.b(new l() {
            final /* synthetic */ l Dv;

            {
                this.Dv = r1;
            }

            protected void a(n nVar) throws RemoteException {
                nVar.eT().a(new ak(this));
            }
        });
    }
}
