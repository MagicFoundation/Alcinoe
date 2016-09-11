package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.metadata.internal.MetadataBundle;

public class OnMetadataResponse implements SafeParcelable {
    public static final Creator<OnMetadataResponse> CREATOR;
    final MetadataBundle Ds;
    final int wj;

    static {
        CREATOR = new af();
    }

    OnMetadataResponse(int versionCode, MetadataBundle metadata) {
        this.wj = versionCode;
        this.Ds = metadata;
    }

    public int describeContents() {
        return 0;
    }

    public MetadataBundle fe() {
        return this.Ds;
    }

    public void writeToParcel(Parcel dest, int flags) {
        af.a(this, dest, flags);
    }
}
