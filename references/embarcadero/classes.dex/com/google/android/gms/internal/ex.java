package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class ex implements Creator<ew> {
    static void a(ew ewVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, ewVar.getVersionCode());
        b.a(parcel, 2, ewVar.ei(), i, false);
        b.D(parcel, p);
    }

    public ew[] T(int i) {
        return new ew[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return q(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return T(x0);
    }

    public ew q(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        ey eyVar = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    eyVar = (ey) a.a(parcel, n, ey.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new ew(i, eyVar);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }
}
