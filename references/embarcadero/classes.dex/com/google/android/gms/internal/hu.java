package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class hu implements Creator<ht> {
    static void a(ht htVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.a(parcel, 1, htVar.Oc, false);
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, htVar.wj);
        b.D(parcel, p);
    }

    public ht az(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        String str = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    str = a.m(parcel, n);
                    break;
                case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE /*1000*/:
                    i = a.g(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new ht(i, str);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public ht[] bt(int i) {
        return new ht[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return az(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bt(x0);
    }
}
