package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class e implements Creator<d> {
    static void a(d dVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, dVar.getVersionCode());
        b.a(parcel, 2, dVar.Yj, i, false);
        b.D(parcel, p);
    }

    public d aV(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        LoyaltyWalletObject loyaltyWalletObject = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    loyaltyWalletObject = (LoyaltyWalletObject) a.a(parcel, n, LoyaltyWalletObject.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new d(i, loyaltyWalletObject);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public d[] cb(int i) {
        return new d[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aV(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return cb(x0);
    }
}
