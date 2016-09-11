package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public final class InstrumentInfo implements SafeParcelable {
    public static final Creator<InstrumentInfo> CREATOR;
    private String Yw;
    private String Yx;
    private final int wj;

    static {
        CREATOR = new h();
    }

    InstrumentInfo(int versionCode, String instrumentType, String instrumentDetails) {
        this.wj = versionCode;
        this.Yw = instrumentType;
        this.Yx = instrumentDetails;
    }

    public int describeContents() {
        return 0;
    }

    public String getInstrumentDetails() {
        return this.Yx;
    }

    public String getInstrumentType() {
        return this.Yw;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel out, int flags) {
        h.a(this, out, flags);
    }
}
