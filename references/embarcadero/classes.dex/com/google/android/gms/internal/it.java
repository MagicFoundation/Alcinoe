package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.internal.ir.a;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.HashSet;
import java.util.Set;

public class it implements Creator<a> {
    static void a(a aVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        Set hB = aVar.hB();
        if (hB.contains(Integer.valueOf(1))) {
            b.c(parcel, 1, aVar.getVersionCode());
        }
        if (hB.contains(Integer.valueOf(2))) {
            b.c(parcel, 2, aVar.getMax());
        }
        if (hB.contains(Integer.valueOf(3))) {
            b.c(parcel, 3, aVar.getMin());
        }
        b.D(parcel, p);
    }

    public a aJ(Parcel parcel) {
        int i = 0;
        int o = com.google.android.gms.common.internal.safeparcel.a.o(parcel);
        Set hashSet = new HashSet();
        int i2 = 0;
        int i3 = 0;
        while (parcel.dataPosition() < o) {
            int n = com.google.android.gms.common.internal.safeparcel.a.n(parcel);
            switch (com.google.android.gms.common.internal.safeparcel.a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i3 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    hashSet.add(Integer.valueOf(1));
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    i2 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    hashSet.add(Integer.valueOf(2));
                    break;
                case DetectedActivity.STILL /*3*/:
                    i = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    hashSet.add(Integer.valueOf(3));
                    break;
                default:
                    com.google.android.gms.common.internal.safeparcel.a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new a(hashSet, i3, i2, i);
        }
        throw new com.google.android.gms.common.internal.safeparcel.a.a("Overread allowed size end=" + o, parcel);
    }

    public a[] bG(int i) {
        return new a[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aJ(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bG(x0);
    }
}
