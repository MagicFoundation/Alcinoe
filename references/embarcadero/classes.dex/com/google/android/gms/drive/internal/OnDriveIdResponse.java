package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.DriveId;

public class OnDriveIdResponse implements SafeParcelable {
    public static final Creator<OnDriveIdResponse> CREATOR;
    DriveId Do;
    final int wj;

    static {
        CREATOR = new ab();
    }

    OnDriveIdResponse(int versionCode, DriveId driveId) {
        this.wj = versionCode;
        this.Do = driveId;
    }

    public int describeContents() {
        return 0;
    }

    public DriveId getDriveId() {
        return this.Do;
    }

    public void writeToParcel(Parcel dest, int flags) {
        ab.a(this, dest, flags);
    }
}
