package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.internal.fe.a;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;

public class fg implements Creator<a> {
    static void a(a aVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, aVar.versionCode);
        b.a(parcel, 2, aVar.className, false);
        b.b(parcel, 3, aVar.CH, false);
        b.D(parcel, p);
    }

    public a[] Z(int i) {
        return new a[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return w(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return Z(x0);
    }

    public a w(Parcel parcel) {
        ArrayList arrayList = null;
        int o = com.google.android.gms.common.internal.safeparcel.a.o(parcel);
        int i = 0;
        String str = null;
        while (parcel.dataPosition() < o) {
            int n = com.google.android.gms.common.internal.safeparcel.a.n(parcel);
            switch (com.google.android.gms.common.internal.safeparcel.a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    arrayList = com.google.android.gms.common.internal.safeparcel.a.c(parcel, n, fe.b.CREATOR);
                    break;
                default:
                    com.google.android.gms.common.internal.safeparcel.a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new a(i, str, arrayList);
        }
        throw new com.google.android.gms.common.internal.safeparcel.a.a("Overread allowed size end=" + o, parcel);
    }
}
