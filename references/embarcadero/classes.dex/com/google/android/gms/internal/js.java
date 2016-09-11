package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class js implements Creator<jr> {
    static void a(jr jrVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, jrVar.getVersionCode());
        b.a(parcel, 2, jrVar.ZL);
        b.a(parcel, 3, jrVar.ZM);
        b.D(parcel, p);
    }

    public jr bl(Parcel parcel) {
        long j = 0;
        int o = a.o(parcel);
        int i = 0;
        long j2 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    j2 = a.h(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    j = a.h(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new jr(i, j2, j);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public jr[] cr(int i) {
        return new jr[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return bl(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return cr(x0);
    }
}
