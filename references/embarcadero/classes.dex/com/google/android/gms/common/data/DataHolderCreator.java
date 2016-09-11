package com.google.android.gms.common.data;

import android.database.CursorWindow;
import android.os.Bundle;
import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class DataHolderCreator implements Creator<DataHolder> {
    public static final int CONTENT_DESCRIPTION = 0;

    static void a(DataHolder dataHolder, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.a(parcel, 1, dataHolder.dH(), false);
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, dataHolder.getVersionCode());
        b.a(parcel, 2, dataHolder.dI(), i, false);
        b.c(parcel, 3, dataHolder.getStatusCode());
        b.a(parcel, 4, dataHolder.getMetadata(), false);
        b.D(parcel, p);
    }

    public DataHolder createFromParcel(Parcel parcel) {
        int i = 0;
        Bundle bundle = null;
        int o = a.o(parcel);
        CursorWindow[] cursorWindowArr = null;
        String[] strArr = null;
        int i2 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    strArr = a.x(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    cursorWindowArr = (CursorWindow[]) a.b(parcel, n, CursorWindow.CREATOR);
                    break;
                case DetectedActivity.STILL /*3*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    bundle = a.o(parcel, n);
                    break;
                case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE /*1000*/:
                    i2 = a.g(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() != o) {
            throw new a.a("Overread allowed size end=" + o, parcel);
        }
        DataHolder dataHolder = new DataHolder(i2, strArr, cursorWindowArr, i, bundle);
        dataHolder.validateContents();
        return dataHolder;
    }

    public DataHolder[] newArray(int size) {
        return new DataHolder[size];
    }
}
