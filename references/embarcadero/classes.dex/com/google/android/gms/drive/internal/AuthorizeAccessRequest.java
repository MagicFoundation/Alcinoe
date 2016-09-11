package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.DriveId;

public class AuthorizeAccessRequest implements SafeParcelable {
    public static final Creator<AuthorizeAccessRequest> CREATOR;
    final DriveId CS;
    final long Dn;
    final int wj;

    static {
        CREATOR = new b();
    }

    AuthorizeAccessRequest(int versionCode, long appId, DriveId driveId) {
        this.wj = versionCode;
        this.Dn = appId;
        this.CS = driveId;
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel dest, int flags) {
        b.a(this, dest, flags);
    }
}
