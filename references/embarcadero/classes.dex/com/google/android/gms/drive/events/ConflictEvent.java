package com.google.android.gms.drive.events;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.DriveId;

public final class ConflictEvent implements SafeParcelable, DriveEvent {
    public static final Creator<ConflictEvent> CREATOR;
    final DriveId CS;
    final int wj;

    static {
        CREATOR = new b();
    }

    ConflictEvent(int versionCode, DriveId driveId) {
        this.wj = versionCode;
        this.CS = driveId;
    }

    public int describeContents() {
        return 0;
    }

    public int getType() {
        return 1;
    }

    public String toString() {
        return String.format("ConflictEvent [id=%s]", new Object[]{this.CS});
    }

    public void writeToParcel(Parcel dest, int flags) {
        b.a(this, dest, flags);
    }
}
