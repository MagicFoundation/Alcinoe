package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.Contents;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.drive.metadata.internal.MetadataBundle;
import com.google.android.gms.internal.er;

public class CreateFileRequest implements SafeParcelable {
    public static final Creator<CreateFileRequest> CREATOR;
    final Contents Dq;
    final MetadataBundle Ds;
    final DriveId Dt;
    final int wj;

    static {
        CREATOR = new h();
    }

    CreateFileRequest(int versionCode, DriveId parentDriveId, MetadataBundle metadata, Contents contentsReference) {
        this.wj = versionCode;
        this.Dt = (DriveId) er.f(parentDriveId);
        this.Ds = (MetadataBundle) er.f(metadata);
        this.Dq = (Contents) er.f(contentsReference);
    }

    public CreateFileRequest(DriveId parentDriveId, MetadataBundle metadata, Contents contentsReference) {
        this(1, parentDriveId, metadata, contentsReference);
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel dest, int flags) {
        h.a(this, dest, flags);
    }
}
