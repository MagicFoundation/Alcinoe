package com.google.android.gms.drive.internal;

import android.os.RemoteException;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.drive.Contents;
import com.google.android.gms.drive.Drive;
import com.google.android.gms.drive.DriveApi.ContentsResult;
import com.google.android.gms.drive.DriveFile;
import com.google.android.gms.drive.DriveFile.DownloadProgressListener;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.drive.MetadataChangeSet;

public class o extends r implements DriveFile {

    private abstract class a extends m<Status> {
        final /* synthetic */ o DJ;

        private a(o oVar) {
            this.DJ = oVar;
        }

        public /* synthetic */ Result d(Status status) {
            return f(status);
        }

        public Status f(Status status) {
            return status;
        }
    }

    private abstract class b extends m<Status> {
        final /* synthetic */ o DJ;

        private b(o oVar) {
            this.DJ = oVar;
        }

        public /* synthetic */ Result d(Status status) {
            return f(status);
        }

        public Status f(Status status) {
            return status;
        }
    }

    private static class c extends c {
        private final DownloadProgressListener DL;
        private final com.google.android.gms.common.api.a.c<ContentsResult> vj;

        public c(com.google.android.gms.common.api.a.c<ContentsResult> cVar, DownloadProgressListener downloadProgressListener) {
            this.vj = cVar;
            this.DL = downloadProgressListener;
        }

        public void a(OnContentsResponse onContentsResponse) throws RemoteException {
            this.vj.b(new a(Status.zQ, onContentsResponse.eX()));
        }

        public void a(OnDownloadProgressResponse onDownloadProgressResponse) throws RemoteException {
            if (this.DL != null) {
                this.DL.onProgress(onDownloadProgressResponse.eY(), onDownloadProgressResponse.eZ());
            }
        }

        public void l(Status status) throws RemoteException {
            this.vj.b(new a(status, null));
        }
    }

    private abstract class d extends m<ContentsResult> {
        final /* synthetic */ o DJ;

        private d(o oVar) {
            this.DJ = oVar;
        }

        public /* synthetic */ Result d(Status status) {
            return n(status);
        }

        public ContentsResult n(Status status) {
            return new a(status, null);
        }
    }

    /* renamed from: com.google.android.gms.drive.internal.o.1 */
    class AnonymousClass1 extends d {
        final /* synthetic */ int DH;
        final /* synthetic */ DownloadProgressListener DI;
        final /* synthetic */ o DJ;

        AnonymousClass1(o oVar, int i, DownloadProgressListener downloadProgressListener) {
            this.DJ = oVar;
            this.DH = i;
            this.DI = downloadProgressListener;
            super(null);
        }

        protected void a(n nVar) throws RemoteException {
            nVar.eT().a(new OpenContentsRequest(this.DJ.getDriveId(), this.DH), new c(this, this.DI));
        }
    }

    /* renamed from: com.google.android.gms.drive.internal.o.2 */
    class AnonymousClass2 extends b {
        final /* synthetic */ o DJ;
        final /* synthetic */ Contents Dw;

        AnonymousClass2(o oVar, Contents contents) {
            this.DJ = oVar;
            this.Dw = contents;
            super(null);
        }

        protected void a(n nVar) throws RemoteException {
            this.Dw.close();
            nVar.eT().a(new CloseContentsRequest(this.Dw, true), new ak(this));
        }
    }

    /* renamed from: com.google.android.gms.drive.internal.o.3 */
    class AnonymousClass3 extends a {
        final /* synthetic */ o DJ;
        final /* synthetic */ MetadataChangeSet DK;
        final /* synthetic */ Contents Dw;

        AnonymousClass3(o oVar, Contents contents, MetadataChangeSet metadataChangeSet) {
            this.DJ = oVar;
            this.Dw = contents;
            this.DK = metadataChangeSet;
            super(null);
        }

        protected void a(n nVar) throws RemoteException {
            this.Dw.close();
            nVar.eT().a(new CloseContentsAndUpdateMetadataRequest(this.DJ.CS, this.DK.eS(), this.Dw), new ak(this));
        }
    }

    public o(DriveId driveId) {
        super(driveId);
    }

    public PendingResult<Status> commitAndCloseContents(GoogleApiClient apiClient, Contents contents) {
        if (contents != null) {
            return apiClient.b(new AnonymousClass2(this, contents));
        }
        throw new IllegalArgumentException("Contents must be provided.");
    }

    public PendingResult<Status> commitAndCloseContents(GoogleApiClient apiClient, Contents contents, MetadataChangeSet changeSet) {
        if (contents != null) {
            return apiClient.b(new AnonymousClass3(this, contents, changeSet));
        }
        throw new IllegalArgumentException("Contents must be provided.");
    }

    public PendingResult<Status> discardContents(GoogleApiClient apiClient, Contents contents) {
        return Drive.DriveApi.discardContents(apiClient, contents);
    }

    public PendingResult<ContentsResult> openContents(GoogleApiClient apiClient, int mode, DownloadProgressListener listener) {
        if (mode == DriveFile.MODE_READ_ONLY || mode == DriveFile.MODE_WRITE_ONLY || mode == DriveFile.MODE_READ_WRITE) {
            return apiClient.a(new AnonymousClass1(this, mode, listener));
        }
        throw new IllegalArgumentException("Invalid mode provided.");
    }
}
