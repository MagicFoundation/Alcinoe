package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.List;

public class ho implements Creator<hn> {
    static void a(hn hnVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.b(parcel, 1, hnVar.LA, false);
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, hnVar.wj);
        b.a(parcel, 2, hnVar.gr(), false);
        b.a(parcel, 3, hnVar.gs());
        b.D(parcel, p);
    }

    public hn aw(Parcel parcel) {
        String str = null;
        boolean z = false;
        int o = a.o(parcel);
        List list = null;
        int i = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    list = a.c(parcel, n, ht.CREATOR);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str = a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    z = a.c(parcel, n);
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
            return new hn(i, list, str, z);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public hn[] bq(int i) {
        return new hn[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aw(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bq(x0);
    }
}
