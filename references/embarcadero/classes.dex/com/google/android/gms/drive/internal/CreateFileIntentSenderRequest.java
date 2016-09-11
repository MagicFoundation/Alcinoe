package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.drive.metadata.internal.MetadataBundle;

public class CreateFileIntentSenderRequest implements SafeParcelable {
    public static final Creator<CreateFileIntentSenderRequest> CREATOR;
    final int CQ;
    final String CX;
    final DriveId CY;
    final MetadataBundle Ds;
    final int wj;

    static {
        CREATOR = new g();
    }

    CreateFileIntentSenderRequest(int versionCode, MetadataBundle metadata, int requestId, String title, DriveId startFolder) {
        this.wj = versionCode;
        this.Ds = metadata;
        this.CQ = requestId;
        this.CX = title;
        this.CY = startFolder;
    }

    public CreateFileIntentSenderRequest(MetadataBundle metadata, int requestId, String title, DriveId startFolder) {
        this(1, metadata, requestId, title, startFolder);
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel dest, int flags) {
        g.a(this, dest, flags);
    }
}
