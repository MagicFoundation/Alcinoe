package com.google.android.gms.maps.model;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.b;

public class d {
    static void a(LatLngBounds latLngBounds, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, latLngBounds.getVersionCode());
        b.a(parcel, 2, latLngBounds.southwest, i, false);
        b.a(parcel, 3, latLngBounds.northeast, i, false);
        b.D(parcel, p);
    }
}
