package com.google.android.gms.location;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class c implements Creator<b> {
    static void a(b bVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, bVar.Lh);
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, bVar.getVersionCode());
        b.c(parcel, 2, bVar.Li);
        b.a(parcel, 3, bVar.Lj);
        b.D(parcel, p);
    }

    public b au(Parcel parcel) {
        int i = 1;
        int o = a.o(parcel);
        int i2 = 0;
        long j = 0;
        int i3 = 1;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i3 = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    j = a.h(parcel, n);
                    break;
                case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE /*1000*/:
                    i2 = a.g(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new b(i2, i3, i, j);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public b[] bm(int i) {
        return new b[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return au(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bm(x0);
    }
}
