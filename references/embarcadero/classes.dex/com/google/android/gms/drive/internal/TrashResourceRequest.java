package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.DriveId;

public class TrashResourceRequest implements SafeParcelable {
    public static final Creator<TrashResourceRequest> CREATOR;
    final DriveId Do;
    final int wj;

    static {
        CREATOR = new al();
    }

    TrashResourceRequest(int versionCode, DriveId id) {
        this.wj = versionCode;
        this.Do = id;
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel dest, int flags) {
        al.a(this, dest, flags);
    }
}
