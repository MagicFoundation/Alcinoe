package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;

public class b implements Creator<Cart> {
    static void a(Cart cart, Parcel parcel, int i) {
        int p = com.google.android.gms.common.internal.safeparcel.b.p(parcel);
        com.google.android.gms.common.internal.safeparcel.b.c(parcel, 1, cart.getVersionCode());
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 2, cart.Yf, false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 3, cart.Yg, false);
        com.google.android.gms.common.internal.safeparcel.b.b(parcel, 4, cart.Yh, false);
        com.google.android.gms.common.internal.safeparcel.b.D(parcel, p);
    }

    public Cart aT(Parcel parcel) {
        String str = null;
        int o = a.o(parcel);
        int i = 0;
        ArrayList arrayList = new ArrayList();
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
                    arrayList = a.c(parcel, n, LineItem.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new Cart(i, str2, str, arrayList);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public Cart[] bZ(int i) {
        return new Cart[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aT(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bZ(x0);
    }
}
