package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class f implements Creator<CreateContentsRequest> {
    static void a(CreateContentsRequest createContentsRequest, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, createContentsRequest.wj);
        b.D(parcel, p);
    }

    public CreateContentsRequest G(Parcel parcel) {
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
            return new CreateContentsRequest(i);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public CreateContentsRequest[] al(int i) {
        return new CreateContentsRequest[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return G(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return al(x0);
    }
}
