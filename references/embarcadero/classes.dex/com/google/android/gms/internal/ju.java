package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class ju implements Creator<jt> {
    static void a(jt jtVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, jtVar.getVersionCode());
        b.a(parcel, 2, jtVar.ZN, false);
        b.a(parcel, 3, jtVar.description, false);
        b.D(parcel, p);
    }

    public jt bm(Parcel parcel) {
        String str = null;
        int o = a.o(parcel);
        int i = 0;
        String str2 = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str2 = a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    str = a.m(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new jt(i, str2, str);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return bm(x0);
    }

    public jt[] cs(int i) {
        return new jt[i];
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return cs(x0);
    }
}
