package com.google.android.gms.drive.events;

import com.google.android.gms.drive.DriveId;

public class c {
    public static boolean a(int i, DriveId driveId) {
        return (driveId == null && (4 & ((long) (1 << i))) == 0) ? false : true;
    }
}
