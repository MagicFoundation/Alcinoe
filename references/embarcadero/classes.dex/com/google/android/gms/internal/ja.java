package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.internal.ir.g;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.HashSet;
import java.util.Set;

public class ja implements Creator<g> {
    static void a(g gVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        Set hB = gVar.hB();
        if (hB.contains(Integer.valueOf(1))) {
            b.c(parcel, 1, gVar.getVersionCode());
        }
        if (hB.contains(Integer.valueOf(2))) {
            b.a(parcel, 2, gVar.isPrimary());
        }
        if (hB.contains(Integer.valueOf(3))) {
            b.a(parcel, 3, gVar.getValue(), true);
        }
        b.D(parcel, p);
    }

    public g aQ(Parcel parcel) {
        boolean z = false;
        int o = a.o(parcel);
        Set hashSet = new HashSet();
        String str = null;
        int i = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(1));
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    z = a.c(parcel, n);
                    hashSet.add(Integer.valueOf(2));
                    break;
                case DetectedActivity.STILL /*3*/:
                    str = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(3));
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new g(hashSet, i, z, str);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public g[] bN(int i) {
        return new g[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aQ(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bN(x0);
    }
}
