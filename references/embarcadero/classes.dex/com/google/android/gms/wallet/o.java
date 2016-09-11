package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class o implements Creator<ProxyCard> {
    static void a(ProxyCard proxyCard, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, proxyCard.getVersionCode());
        b.a(parcel, 2, proxyCard.Zn, false);
        b.a(parcel, 3, proxyCard.Zo, false);
        b.c(parcel, 4, proxyCard.Zp);
        b.c(parcel, 5, proxyCard.Zq);
        b.D(parcel, p);
    }

    public ProxyCard bf(Parcel parcel) {
        String str = null;
        int i = 0;
        int o = a.o(parcel);
        int i2 = 0;
        String str2 = null;
        int i3 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i3 = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str2 = a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    str = a.m(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    i2 = a.g(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    i = a.g(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new ProxyCard(i3, str2, str, i2, i);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public ProxyCard[] cl(int i) {
        return new ProxyCard[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return bf(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return cl(x0);
    }
}
