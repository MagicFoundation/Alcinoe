package com.google.android.gms.drive;

import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.drive.DriveApi.MetadataBufferResult;
import com.google.android.gms.drive.events.ChangeEvent;
import com.google.android.gms.drive.events.DriveEvent.Listener;

public interface DriveResource {

    public interface MetadataResult extends Result {
        Metadata getMetadata();
    }

    PendingResult<Status> addChangeListener(GoogleApiClient googleApiClient, Listener<ChangeEvent> listener);

    DriveId getDriveId();

    PendingResult<MetadataResult> getMetadata(GoogleApiClient googleApiClient);

    PendingResult<MetadataBufferResult> listParents(GoogleApiClient googleApiClient);

    PendingResult<Status> removeChangeListener(GoogleApiClient googleApiClient, Listener<ChangeEvent> listener);

    PendingResult<MetadataResult> updateMetadata(GoogleApiClient googleApiClient, MetadataChangeSet metadataChangeSet);
}
