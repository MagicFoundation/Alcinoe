package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class ji implements Creator<jh> {
    static void a(jh jhVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, jhVar.getVersionCode());
        b.a(parcel, 2, jhVar.label, false);
        b.a(parcel, 3, jhVar.value, false);
        b.D(parcel, p);
    }

    public jh bg(Parcel parcel) {
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
            return new jh(i, str2, str);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public jh[] cm(int i) {
        return new jh[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return bg(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return cm(x0);
    }
}
