package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;

public class ff implements Creator<fe> {
    static void a(fe feVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, feVar.getVersionCode());
        b.b(parcel, 2, feVar.eC(), false);
        b.a(parcel, 3, feVar.eD(), false);
        b.D(parcel, p);
    }

    public fe[] Y(int i) {
        return new fe[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return v(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return Y(x0);
    }

    public fe v(Parcel parcel) {
        String str = null;
        int o = a.o(parcel);
        int i = 0;
        ArrayList arrayList = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    arrayList = a.c(parcel, n, fe.a.CREATOR);
                    break;
                case DetectedActivity.STILL /*3*/:
                    str = a.m(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new fe(i, arrayList, str);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }
}
