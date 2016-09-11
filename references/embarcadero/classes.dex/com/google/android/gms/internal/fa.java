package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.internal.ey.a;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class fa implements Creator<a> {
    static void a(a aVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, aVar.versionCode);
        b.a(parcel, 2, aVar.Cs, false);
        b.c(parcel, 3, aVar.Ct);
        b.D(parcel, p);
    }

    public a[] V(int i) {
        return new a[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return s(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return V(x0);
    }

    public a s(Parcel parcel) {
        int i = 0;
        int o = com.google.android.gms.common.internal.safeparcel.a.o(parcel);
        String str = null;
        int i2 = 0;
        while (parcel.dataPosition() < o) {
            int n = com.google.android.gms.common.internal.safeparcel.a.n(parcel);
            switch (com.google.android.gms.common.internal.safeparcel.a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i2 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    i = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                default:
                    com.google.android.gms.common.internal.safeparcel.a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new a(i2, str, i);
        }
        throw new com.google.android.gms.common.internal.safeparcel.a.a("Overread allowed size end=" + o, parcel);
    }
}
