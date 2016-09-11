package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class fi implements Creator<fh> {
    static void a(fh fhVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, fhVar.getVersionCode());
        b.a(parcel, 2, fhVar.eF(), false);
        b.a(parcel, 3, fhVar.eG(), i, false);
        b.D(parcel, p);
    }

    public fh[] aa(int i) {
        return new fh[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return x(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return aa(x0);
    }

    public fh x(Parcel parcel) {
        fe feVar = null;
        int o = a.o(parcel);
        int i = 0;
        Parcel parcel2 = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    parcel2 = a.z(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    feVar = (fe) a.a(parcel, n, fe.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new fh(i, parcel2, feVar);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }
}
