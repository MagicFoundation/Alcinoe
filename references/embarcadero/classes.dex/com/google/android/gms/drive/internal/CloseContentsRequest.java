package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.Contents;

public class CloseContentsRequest implements SafeParcelable {
    public static final Creator<CloseContentsRequest> CREATOR;
    final Contents Dq;
    final Boolean Dr;
    final int wj;

    static {
        CREATOR = new e();
    }

    CloseContentsRequest(int versionCode, Contents contentsReference, Boolean saveResults) {
        this.wj = versionCode;
        this.Dq = contentsReference;
        this.Dr = saveResults;
    }

    public CloseContentsRequest(Contents contentsReference, boolean saveResults) {
        this(1, contentsReference, Boolean.valueOf(saveResults));
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel dest, int flags) {
        e.a(this, dest, flags);
    }
}
