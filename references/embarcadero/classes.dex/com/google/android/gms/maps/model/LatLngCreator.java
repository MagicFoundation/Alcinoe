package com.google.android.gms.maps.model;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class LatLngCreator implements Creator<LatLng> {
    public static final int CONTENT_DESCRIPTION = 0;

    static void a(LatLng latLng, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, latLng.getVersionCode());
        b.a(parcel, 2, latLng.latitude);
        b.a(parcel, 3, latLng.longitude);
        b.D(parcel, p);
    }

    public LatLng createFromParcel(Parcel parcel) {
        double d = 0.0d;
        int o = a.o(parcel);
        int i = 0;
        double d2 = 0.0d;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    d2 = a.k(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    d = a.k(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new LatLng(i, d2, d);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public LatLng[] newArray(int size) {
        return new LatLng[size];
    }
}
