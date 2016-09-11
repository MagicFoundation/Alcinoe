package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class dc implements Creator<db> {
    static void a(db dbVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, dbVar.versionCode);
        b.a(parcel, 2, dbVar.pU, false);
        b.c(parcel, 3, dbVar.pV);
        b.c(parcel, 4, dbVar.pW);
        b.a(parcel, 5, dbVar.pX);
        b.D(parcel, p);
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return h(x0);
    }

    public db h(Parcel parcel) {
        boolean z = false;
        int o = a.o(parcel);
        String str = null;
        int i = 0;
        int i2 = 0;
        int i3 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i3 = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str = a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    i2 = a.g(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    z = a.c(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new db(i3, str, i2, i, z);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return o(x0);
    }

    public db[] o(int i) {
        return new db[i];
    }
}
