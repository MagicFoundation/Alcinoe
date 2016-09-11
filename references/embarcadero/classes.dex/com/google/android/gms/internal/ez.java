package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;

public class ez implements Creator<ey> {
    static void a(ey eyVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, eyVar.getVersionCode());
        b.b(parcel, 2, eyVar.ek(), false);
        b.D(parcel, p);
    }

    public ey[] U(int i) {
        return new ey[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return r(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return U(x0);
    }

    public ey r(Parcel parcel) {
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
                    arrayList = a.c(parcel, n, ey.a.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new ey(i, arrayList);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }
}
