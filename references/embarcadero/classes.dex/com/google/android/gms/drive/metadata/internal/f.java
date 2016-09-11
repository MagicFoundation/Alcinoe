package com.google.android.gms.drive.metadata.internal;

import android.os.Bundle;
import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class f implements Creator<MetadataBundle> {
    static void a(MetadataBundle metadataBundle, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, metadataBundle.wj);
        b.a(parcel, 2, metadataBundle.Ek, false);
        b.D(parcel, p);
    }

    public MetadataBundle[] aF(int i) {
        return new MetadataBundle[i];
    }

    public MetadataBundle aa(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        Bundle bundle = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    bundle = a.o(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new MetadataBundle(i, bundle);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aa(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return aF(x0);
    }
}
