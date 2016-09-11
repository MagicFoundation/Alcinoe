package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.DriveId;

public class OpenFileIntentSenderRequest implements SafeParcelable {
    public static final Creator<OpenFileIntentSenderRequest> CREATOR;
    final String CX;
    final DriveId CY;
    final String[] Dk;
    final int wj;

    static {
        CREATOR = new ah();
    }

    OpenFileIntentSenderRequest(int versionCode, String title, String[] mimeTypes, DriveId startFolder) {
        this.wj = versionCode;
        this.CX = title;
        this.Dk = mimeTypes;
        this.CY = startFolder;
    }

    public OpenFileIntentSenderRequest(String title, String[] mimeTypes, DriveId startFolder) {
        this(1, title, mimeTypes, startFolder);
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel dest, int flags) {
        ah.a(this, dest, flags);
    }
}
