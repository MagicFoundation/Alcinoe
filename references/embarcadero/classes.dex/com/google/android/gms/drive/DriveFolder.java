package com.google.android.gms.drive;

import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.drive.DriveApi.MetadataBufferResult;
import com.google.android.gms.drive.query.Query;

public interface DriveFolder extends DriveResource {
    public static final String MIME_TYPE = "application/vnd.google-apps.folder";

    public interface DriveFileResult extends Result {
        DriveFile getDriveFile();
    }

    public interface DriveFolderResult extends Result {
        DriveFolder getDriveFolder();
    }

    PendingResult<DriveFileResult> createFile(GoogleApiClient googleApiClient, MetadataChangeSet metadataChangeSet, Contents contents);

    PendingResult<DriveFolderResult> createFolder(GoogleApiClient googleApiClient, MetadataChangeSet metadataChangeSet);

    PendingResult<MetadataBufferResult> listChildren(GoogleApiClient googleApiClient);

    PendingResult<MetadataBufferResult> queryChildren(GoogleApiClient googleApiClient, Query query);
}
