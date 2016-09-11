package com.google.android.gms.drive;

import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.drive.DriveApi.ContentsResult;

public interface DriveFile extends DriveResource {
    public static final int MODE_READ_ONLY = 268435456;
    public static final int MODE_READ_WRITE = 805306368;
    public static final int MODE_WRITE_ONLY = 536870912;

    public interface DownloadProgressListener {
        void onProgress(long j, long j2);
    }

    PendingResult<Status> commitAndCloseContents(GoogleApiClient googleApiClient, Contents contents);

    PendingResult<Status> commitAndCloseContents(GoogleApiClient googleApiClient, Contents contents, MetadataChangeSet metadataChangeSet);

    PendingResult<Status> discardContents(GoogleApiClient googleApiClient, Contents contents);

    PendingResult<ContentsResult> openContents(GoogleApiClient googleApiClient, int i, DownloadProgressListener downloadProgressListener);
}
