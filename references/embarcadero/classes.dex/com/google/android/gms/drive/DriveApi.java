package com.google.android.gms.drive;

import android.content.IntentSender;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.drive.query.Query;

public interface DriveApi {

    public interface OnSyncFinishCallback {
        void onSyncFinish(Status status);
    }

    public interface ContentsResult extends Result {
        Contents getContents();
    }

    public interface DriveIdResult extends Result {
        DriveId getDriveId();
    }

    public interface IntentSenderResult extends Result {
        IntentSender getIntentSender();
    }

    public interface MetadataBufferResult extends Result {
        MetadataBuffer getMetadataBuffer();
    }

    PendingResult<Status> discardContents(GoogleApiClient googleApiClient, Contents contents);

    PendingResult<DriveIdResult> fetchDriveId(GoogleApiClient googleApiClient, String str);

    DriveFolder getAppFolder(GoogleApiClient googleApiClient);

    DriveFile getFile(GoogleApiClient googleApiClient, DriveId driveId);

    DriveFolder getFolder(GoogleApiClient googleApiClient, DriveId driveId);

    DriveFolder getRootFolder(GoogleApiClient googleApiClient);

    PendingResult<ContentsResult> newContents(GoogleApiClient googleApiClient);

    CreateFileActivityBuilder newCreateFileActivityBuilder();

    OpenFileActivityBuilder newOpenFileActivityBuilder();

    PendingResult<MetadataBufferResult> query(GoogleApiClient googleApiClient, Query query);

    PendingResult<Status> requestSync(GoogleApiClient googleApiClient);
}
