package com.google.android.gms.common.data;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public class c<T extends SafeParcelable> extends DataBuffer<T> {
    private static final String[] zY;
    private final Creator<T> zZ;

    static {
        zY = new String[]{"data"};
    }

    public c(DataHolder dataHolder, Creator<T> creator) {
        super(dataHolder);
        this.zZ = creator;
    }

    public T H(int i) {
        byte[] byteArray = this.zU.getByteArray("data", i, 0);
        Parcel obtain = Parcel.obtain();
        obtain.unmarshall(byteArray, 0, byteArray.length);
        obtain.setDataPosition(0);
        SafeParcelable safeParcelable = (SafeParcelable) this.zZ.createFromParcel(obtain);
        obtain.recycle();
        return safeParcelable;
    }

    public /* synthetic */ Object get(int x0) {
        return H(x0);
    }
}
