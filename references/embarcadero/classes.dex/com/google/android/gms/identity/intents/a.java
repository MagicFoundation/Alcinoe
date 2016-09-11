package com.google.android.gms.identity.intents;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.identity.intents.model.CountrySpecification;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.List;

public class a implements Creator<UserAddressRequest> {
    static void a(UserAddressRequest userAddressRequest, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, userAddressRequest.getVersionCode());
        b.b(parcel, 2, userAddressRequest.Ky, false);
        b.D(parcel, p);
    }

    public UserAddressRequest ar(Parcel parcel) {
        int o = com.google.android.gms.common.internal.safeparcel.a.o(parcel);
        int i = 0;
        List list = null;
        while (parcel.dataPosition() < o) {
            int n = com.google.android.gms.common.internal.safeparcel.a.n(parcel);
            switch (com.google.android.gms.common.internal.safeparcel.a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    list = com.google.android.gms.common.internal.safeparcel.a.c(parcel, n, CountrySpecification.CREATOR);
                    break;
                default:
                    com.google.android.gms.common.internal.safeparcel.a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new UserAddressRequest(i, list);
        }
        throw new com.google.android.gms.common.internal.safeparcel.a.a("Overread allowed size end=" + o, parcel);
    }

    public UserAddressRequest[] be(int i) {
        return new UserAddressRequest[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return ar(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return be(x0);
    }
}
