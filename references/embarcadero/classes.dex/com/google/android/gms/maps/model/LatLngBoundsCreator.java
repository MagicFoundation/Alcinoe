package com.google.android.gms.maps.model;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class LatLngBoundsCreator implements Creator<LatLngBounds> {
    public static final int CONTENT_DESCRIPTION = 0;

    static void a(LatLngBounds latLngBounds, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, latLngBounds.getVersionCode());
        b.a(parcel, 2, latLngBounds.southwest, i, false);
        b.a(parcel, 3, latLngBounds.northeast, i, false);
        b.D(parcel, p);
    }

    public LatLngBounds createFromParcel(Parcel parcel) {
        LatLng latLng = null;
        int o = a.o(parcel);
        int i = 0;
        LatLng latLng2 = null;
        while (parcel.dataPosition() < o) {
            int g;
            LatLng latLng3;
            int n = a.n(parcel);
            LatLng latLng4;
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    latLng4 = latLng;
                    latLng = latLng2;
                    g = a.g(parcel, n);
                    latLng3 = latLng4;
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    g = i;
                    latLng4 = (LatLng) a.a(parcel, n, LatLng.CREATOR);
                    latLng3 = latLng;
                    latLng = latLng4;
                    break;
                case DetectedActivity.STILL /*3*/:
                    latLng3 = (LatLng) a.a(parcel, n, LatLng.CREATOR);
                    latLng = latLng2;
                    g = i;
                    break;
                default:
                    a.b(parcel, n);
                    latLng3 = latLng;
                    latLng = latLng2;
                    g = i;
                    break;
            }
            i = g;
            latLng2 = latLng;
            latLng = latLng3;
        }
        if (parcel.dataPosition() == o) {
            return new LatLngBounds(i, latLng2, latLng);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public LatLngBounds[] newArray(int size) {
        return new LatLngBounds[size];
    }
}
