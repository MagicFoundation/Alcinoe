package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.events.ChangeEvent;
import com.google.android.gms.drive.events.ConflictEvent;

public class OnEventResponse implements SafeParcelable {
    public static final Creator<OnEventResponse> CREATOR;
    final int Dm;
    final ChangeEvent Eb;
    final ConflictEvent Ec;
    final int wj;

    static {
        CREATOR = new ac();
    }

    OnEventResponse(int versionCode, int eventType, ChangeEvent changeEvent, ConflictEvent conflictEvent) {
        this.wj = versionCode;
        this.Dm = eventType;
        this.Eb = changeEvent;
        this.Ec = conflictEvent;
    }

    public int describeContents() {
        return 0;
    }

    public ChangeEvent fa() {
        return this.Eb;
    }

    public ConflictEvent fb() {
        return this.Ec;
    }

    public int getEventType() {
        return this.Dm;
    }

    public void writeToParcel(Parcel dest, int flags) {
        ac.a(this, dest, flags);
    }
}
