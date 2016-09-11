package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class h implements Creator<InstrumentInfo> {
    static void a(InstrumentInfo instrumentInfo, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, instrumentInfo.getVersionCode());
        b.a(parcel, 2, instrumentInfo.getInstrumentType(), false);
        b.a(parcel, 3, instrumentInfo.getInstrumentDetails(), false);
        b.D(parcel, p);
    }

    public InstrumentInfo aY(Parcel parcel) {
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
            return new InstrumentInfo(i, str2, str);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public InstrumentInfo[] ce(int i) {
        return new InstrumentInfo[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aY(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return ce(x0);
    }
}
