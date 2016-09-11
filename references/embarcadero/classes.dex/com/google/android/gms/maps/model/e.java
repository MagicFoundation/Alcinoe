package com.google.android.gms.maps.model;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.b;

public class e {
    static void a(LatLng latLng, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, latLng.getVersionCode());
        b.a(parcel, 2, latLng.latitude);
        b.a(parcel, 3, latLng.longitude);
        b.D(parcel, p);
    }
}
