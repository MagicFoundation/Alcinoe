package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.drive.metadata.internal.MetadataBundle;

public class UpdateMetadataRequest implements SafeParcelable {
    public static final Creator<UpdateMetadataRequest> CREATOR;
    final DriveId Do;
    final MetadataBundle Dp;
    final int wj;

    static {
        CREATOR = new am();
    }

    UpdateMetadataRequest(int versionCode, DriveId id, MetadataBundle metadataChangeSet) {
        this.wj = versionCode;
        this.Do = id;
        this.Dp = metadataChangeSet;
    }

    public UpdateMetadataRequest(DriveId id, MetadataBundle metadataChangeSet) {
        this(1, id, metadataChangeSet);
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel dest, int flags) {
        am.a(this, dest, flags);
    }
}
