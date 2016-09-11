package com.google.android.gms.internal;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.internal.fb.b;

public class ew implements SafeParcelable {
    public static final ex CREATOR;
    private final ey Co;
    private final int wj;

    static {
        CREATOR = new ex();
    }

    ew(int i, ey eyVar) {
        this.wj = i;
        this.Co = eyVar;
    }

    private ew(ey eyVar) {
        this.wj = 1;
        this.Co = eyVar;
    }

    public static ew a(b<?, ?> bVar) {
        if (bVar instanceof ey) {
            return new ew((ey) bVar);
        }
        throw new IllegalArgumentException("Unsupported safe parcelable field converter class.");
    }

    public int describeContents() {
        ex exVar = CREATOR;
        return 0;
    }

    ey ei() {
        return this.Co;
    }

    public b<?, ?> ej() {
        if (this.Co != null) {
            return this.Co;
        }
        throw new IllegalStateException("There was no converter wrapped in this ConverterWrapper.");
    }

    int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel out, int flags) {
        ex exVar = CREATOR;
        ex.a(this, out, flags);
    }
}
