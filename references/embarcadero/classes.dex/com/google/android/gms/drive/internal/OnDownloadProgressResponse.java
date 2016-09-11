package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public class OnDownloadProgressResponse implements SafeParcelable {
    public static final Creator<OnDownloadProgressResponse> CREATOR;
    final long DZ;
    final long Ea;
    final int wj;

    static {
        CREATOR = new aa();
    }

    OnDownloadProgressResponse(int versionCode, long bytesLoaded, long bytesExpected) {
        this.wj = versionCode;
        this.DZ = bytesLoaded;
        this.Ea = bytesExpected;
    }

    public int describeContents() {
        return 0;
    }

    public long eY() {
        return this.DZ;
    }

    public long eZ() {
        return this.Ea;
    }

    public void writeToParcel(Parcel dest, int flags) {
        aa.a(this, dest, flags);
    }
}
