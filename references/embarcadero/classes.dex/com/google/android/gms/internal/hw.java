package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.internal.hx.a;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class hw implements Creator<a> {
    static void a(a aVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.a(parcel, 1, aVar.gt(), false);
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, aVar.wj);
        b.a(parcel, 2, aVar.getTag(), false);
        b.a(parcel, 3, aVar.gH(), false);
        b.c(parcel, 4, aVar.gI());
        b.D(parcel, p);
    }

    public a aA(Parcel parcel) {
        int i = 0;
        String str = null;
        int o = com.google.android.gms.common.internal.safeparcel.a.o(parcel);
        String str2 = null;
        String str3 = null;
        int i2 = 0;
        while (parcel.dataPosition() < o) {
            int n = com.google.android.gms.common.internal.safeparcel.a.n(parcel);
            switch (com.google.android.gms.common.internal.safeparcel.a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    str3 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str2 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    str = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    i = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE /*1000*/:
                    i2 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                default:
                    com.google.android.gms.common.internal.safeparcel.a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new a(i2, str3, str2, str, i);
        }
        throw new com.google.android.gms.common.internal.safeparcel.a.a("Overread allowed size end=" + o, parcel);
    }

    public a[] bu(int i) {
        return new a[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aA(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bu(x0);
    }
}
