package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class k implements Creator<DisconnectRequest> {
    static void a(DisconnectRequest disconnectRequest, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, disconnectRequest.wj);
        b.D(parcel, p);
    }

    public DisconnectRequest K(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new DisconnectRequest(i);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public DisconnectRequest[] ap(int i) {
        return new DisconnectRequest[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return K(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return ap(x0);
    }
}
