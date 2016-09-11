package com.google.android.gms.drive.internal;

import android.os.RemoteException;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.drive.Contents;
import com.google.android.gms.drive.DriveApi.MetadataBufferResult;
import com.google.android.gms.drive.DriveFile;
import com.google.android.gms.drive.DriveFolder;
import com.google.android.gms.drive.DriveFolder.DriveFileResult;
import com.google.android.gms.drive.DriveFolder.DriveFolderResult;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.drive.MetadataChangeSet;
import com.google.android.gms.drive.query.Filters;
import com.google.android.gms.drive.query.Query;
import com.google.android.gms.drive.query.Query.Builder;
import com.google.android.gms.drive.query.SearchableField;

public class q extends r implements DriveFolder {

    private static class d implements DriveFileResult {
        private final DriveFile DN;
        private final Status vl;

        public d(Status status, DriveFile driveFile) {
            this.vl = status;
            this.DN = driveFile;
        }

        public DriveFile getDriveFile() {
            return this.DN;
        }

        public Status getStatus() {
            return this.vl;
        }
    }

    private static class e implements DriveFolderResult {
        private final DriveFolder DO;
        private final Status vl;

        public e(Status status, DriveFolder driveFolder) {
            this.vl = status;
            this.DO = driveFolder;
        }

        public DriveFolder getDriveFolder() {
            return this.DO;
        }

        public Status getStatus() {
            return this.vl;
        }
    }

    /* renamed from: com.google.android.gms.drive.internal.q.1 */
    class AnonymousClass1 extends m<DriveFileResult> {
        final /* synthetic */ MetadataChangeSet DK;
        final /* synthetic */ q DM;
        final /* synthetic */ Contents Dw;

        AnonymousClass1(q qVar, Contents contents, MetadataChangeSet metadataChangeSet) {
            this.DM = qVar;
            this.Dw = contents;
            this.DK = metadataChangeSet;
        }

        protected void a(n nVar) throws RemoteException {
            this.Dw.close();
            nVar.eT().a(new CreateFileRequest(this.DM.getDriveId(), this.DK.eS(), this.Dw), new a(this));
        }

        public /* synthetic */ Result d(Status status) {
            return p(status);
        }

        public DriveFileResult p(Status status) {
            return new d(status, null);
        }
    }

    private static class a extends c {
        private final com.google.android.gms.common.api.a.c<DriveFileResult> vj;

        public a(com.google.android.gms.common.api.a.c<DriveFileResult> cVar) {
            this.vj = cVar;
        }

        public void a(OnDriveIdResponse onDriveIdResponse) throws RemoteException {
            this.vj.b(new d(Status.zQ, new o(onDriveIdResponse.getDriveId())));
        }

        public void l(Status status) throws RemoteException {
            this.vj.b(new d(status, null));
        }
    }

    private static class b extends c {
        private final com.google.android.gms.common.api.a.c<DriveFolderResult> vj;

        public b(com.google.android.gms.common.api.a.c<DriveFolderResult> cVar) {
            this.vj = cVar;
        }

        public void a(OnDriveIdResponse onDriveIdResponse) throws RemoteException {
            this.vj.b(new e(Status.zQ, new q(onDriveIdResponse.getDriveId())));
        }

        public void l(Status status) throws RemoteException {
            this.vj.b(new e(status, null));
        }
    }

    private abstract class c extends m<DriveFolderResult> {
        final /* synthetic */ q DM;

        private c(q qVar) {
            this.DM = qVar;
        }

        public /* synthetic */ Result d(Status status) {
            return q(status);
        }

        public DriveFolderResult q(Status status) {
            return new e(status, null);
        }
    }

    /* renamed from: com.google.android.gms.drive.internal.q.2 */
    class AnonymousClass2 extends c {
        final /* synthetic */ MetadataChangeSet DK;
        final /* synthetic */ q DM;

        AnonymousClass2(q qVar, MetadataChangeSet metadataChangeSet) {
            this.DM = qVar;
            this.DK = metadataChangeSet;
            super(null);
        }

        protected void a(n nVar) throws RemoteException {
            nVar.eT().a(new CreateFolderRequest(this.DM.getDriveId(), this.DK.eS()), new b(this));
        }
    }

    public q(DriveId driveId) {
        super(driveId);
    }

    public PendingResult<DriveFileResult> createFile(GoogleApiClient apiClient, MetadataChangeSet changeSet, Contents contents) {
        if (changeSet == null) {
            throw new IllegalArgumentException("MetatadataChangeSet must be provided.");
        } else if (contents == null) {
            throw new IllegalArgumentException("Contents must be provided.");
        } else if (!DriveFolder.MIME_TYPE.equals(changeSet.getMimeType())) {
            return apiClient.b(new AnonymousClass1(this, contents, changeSet));
        } else {
            throw new IllegalArgumentException("May not create folders (mimetype: application/vnd.google-apps.folder) using this method. Use DriveFolder.createFolder() instead.");
        }
    }

    public PendingResult<DriveFolderResult> createFolder(GoogleApiClient apiClient, MetadataChangeSet changeSet) {
        if (changeSet == null) {
            throw new IllegalArgumentException("MetatadataChangeSet must be provided.");
        } else if (changeSet.getMimeType() == null || changeSet.getMimeType().equals(DriveFolder.MIME_TYPE)) {
            return apiClient.b(new AnonymousClass2(this, changeSet));
        } else {
            throw new IllegalArgumentException("The mimetype must be of type application/vnd.google-apps.folder");
        }
    }

    public PendingResult<MetadataBufferResult> listChildren(GoogleApiClient apiClient) {
        return queryChildren(apiClient, null);
    }

    public PendingResult<MetadataBufferResult> queryChildren(GoogleApiClient apiClient, Query query) {
        Builder addFilter = new Builder().addFilter(Filters.in(SearchableField.PARENTS, getDriveId()));
        if (query != null) {
            if (query.getFilter() != null) {
                addFilter.addFilter(query.getFilter());
            }
            addFilter.setPageToken(query.getPageToken());
        }
        return new l().query(apiClient, addFilter.build());
    }
}
