package com.google.android.gms.drive.query.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class h implements Creator<Operator> {
    static void a(Operator operator, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, operator.wj);
        b.a(parcel, 1, operator.mTag, false);
        b.D(parcel, p);
    }

    public Operator[] aN(int i) {
        return new Operator[i];
    }

    public Operator ai(Parcel parcel) {
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
            return new Operator(i, str);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return ai(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return aN(x0);
    }
}
