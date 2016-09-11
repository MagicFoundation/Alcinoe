package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class g implements Creator<FullWalletRequest> {
    static void a(FullWalletRequest fullWalletRequest, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, fullWalletRequest.getVersionCode());
        b.a(parcel, 2, fullWalletRequest.Yk, false);
        b.a(parcel, 3, fullWalletRequest.Yl, false);
        b.a(parcel, 4, fullWalletRequest.Yu, i, false);
        b.D(parcel, p);
    }

    public FullWalletRequest aX(Parcel parcel) {
        Cart cart = null;
        int o = a.o(parcel);
        int i = 0;
        String str = null;
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
                    cart = (Cart) a.a(parcel, n, Cart.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new FullWalletRequest(i, str2, str, cart);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public FullWalletRequest[] cd(int i) {
        return new FullWalletRequest[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aX(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return cd(x0);
    }
}
