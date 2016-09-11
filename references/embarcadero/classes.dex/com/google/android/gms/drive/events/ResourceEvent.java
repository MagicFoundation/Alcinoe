package com.google.android.gms.drive.events;

import com.google.android.gms.drive.DriveId;

public interface ResourceEvent extends DriveEvent {
    DriveId getDriveId();
}
