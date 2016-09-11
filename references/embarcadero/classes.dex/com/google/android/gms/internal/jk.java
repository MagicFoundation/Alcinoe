package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;

public class jk implements Creator<jj> {
    static void a(jj jjVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, jjVar.getVersionCode());
        b.a(parcel, 2, jjVar.ZA, false);
        b.a(parcel, 3, jjVar.ZB, false);
        b.b(parcel, 4, jjVar.ZC, false);
        b.D(parcel, p);
    }

    public jj bh(Parcel parcel) {
        String str = null;
        int o = a.o(parcel);
        int i = 0;
        ArrayList eH = fj.eH();
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
                case DetectedActivity.UNKNOWN /*4*/:
                    eH = a.c(parcel, n, jh.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new jj(i, str2, str, eH);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public jj[] cn(int i) {
        return new jj[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return bh(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return cn(x0);
    }
}
