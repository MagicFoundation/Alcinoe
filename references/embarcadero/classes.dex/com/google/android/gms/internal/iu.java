package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.internal.ir.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.HashSet;
import java.util.Set;

public class iu implements Creator<b> {
    static void a(b bVar, Parcel parcel, int i) {
        int p = com.google.android.gms.common.internal.safeparcel.b.p(parcel);
        Set hB = bVar.hB();
        if (hB.contains(Integer.valueOf(1))) {
            com.google.android.gms.common.internal.safeparcel.b.c(parcel, 1, bVar.getVersionCode());
        }
        if (hB.contains(Integer.valueOf(2))) {
            com.google.android.gms.common.internal.safeparcel.b.a(parcel, 2, bVar.if(), i, true);
        }
        if (hB.contains(Integer.valueOf(3))) {
            com.google.android.gms.common.internal.safeparcel.b.a(parcel, 3, bVar.ig(), i, true);
        }
        if (hB.contains(Integer.valueOf(4))) {
            com.google.android.gms.common.internal.safeparcel.b.c(parcel, 4, bVar.getLayout());
        }
        com.google.android.gms.common.internal.safeparcel.b.D(parcel, p);
    }

    public b aK(Parcel parcel) {
        b.b bVar = null;
        int i = 0;
        int o = a.o(parcel);
        Set hashSet = new HashSet();
        b.a aVar = null;
        int i2 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i2 = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(1));
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    b.a aVar2 = (b.a) a.a(parcel, n, b.a.CREATOR);
                    hashSet.add(Integer.valueOf(2));
                    aVar = aVar2;
                    break;
                case DetectedActivity.STILL /*3*/:
                    b.b bVar2 = (b.b) a.a(parcel, n, b.b.CREATOR);
                    hashSet.add(Integer.valueOf(3));
                    bVar = bVar2;
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    i = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(4));
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new b(hashSet, i2, aVar, bVar, i);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public b[] bH(int i) {
        return new b[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aK(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bH(x0);
    }
}
