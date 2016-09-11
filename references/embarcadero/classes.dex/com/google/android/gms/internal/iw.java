package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.internal.ir.b.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.HashSet;
import java.util.Set;

public class iw implements Creator<b> {
    static void a(b bVar, Parcel parcel, int i) {
        int p = com.google.android.gms.common.internal.safeparcel.b.p(parcel);
        Set hB = bVar.hB();
        if (hB.contains(Integer.valueOf(1))) {
            com.google.android.gms.common.internal.safeparcel.b.c(parcel, 1, bVar.getVersionCode());
        }
        if (hB.contains(Integer.valueOf(2))) {
            com.google.android.gms.common.internal.safeparcel.b.c(parcel, 2, bVar.getHeight());
        }
        if (hB.contains(Integer.valueOf(3))) {
            com.google.android.gms.common.internal.safeparcel.b.a(parcel, 3, bVar.getUrl(), true);
        }
        if (hB.contains(Integer.valueOf(4))) {
            com.google.android.gms.common.internal.safeparcel.b.c(parcel, 4, bVar.getWidth());
        }
        com.google.android.gms.common.internal.safeparcel.b.D(parcel, p);
    }

    public b aM(Parcel parcel) {
        int i = 0;
        int o = a.o(parcel);
        Set hashSet = new HashSet();
        String str = null;
        int i2 = 0;
        int i3 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i3 = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(1));
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    i2 = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(2));
                    break;
                case DetectedActivity.STILL /*3*/:
                    str = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(3));
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
            return new b(hashSet, i3, i2, str, i);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public b[] bJ(int i) {
        return new b[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aM(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bJ(x0);
    }
}
