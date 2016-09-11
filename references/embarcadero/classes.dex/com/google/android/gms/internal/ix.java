package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.internal.ir.c;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.HashSet;
import java.util.Set;

public class ix implements Creator<c> {
    static void a(c cVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        Set hB = cVar.hB();
        if (hB.contains(Integer.valueOf(1))) {
            b.c(parcel, 1, cVar.getVersionCode());
        }
        if (hB.contains(Integer.valueOf(2))) {
            b.a(parcel, 2, cVar.getUrl(), true);
        }
        b.D(parcel, p);
    }

    public c aN(Parcel parcel) {
        int o = a.o(parcel);
        Set hashSet = new HashSet();
        int i = 0;
        String str = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(1));
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(2));
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new c(hashSet, i, str);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public c[] bK(int i) {
        return new c[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aN(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bK(x0);
    }
}
