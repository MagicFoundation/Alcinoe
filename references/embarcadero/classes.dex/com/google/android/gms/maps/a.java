package com.google.android.gms.maps;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.b;

public class a {
    static void a(GoogleMapOptions googleMapOptions, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, googleMapOptions.getVersionCode());
        b.a(parcel, 2, googleMapOptions.gN());
        b.a(parcel, 3, googleMapOptions.gO());
        b.c(parcel, 4, googleMapOptions.getMapType());
        b.a(parcel, 5, googleMapOptions.getCamera(), i, false);
        b.a(parcel, 6, googleMapOptions.gP());
        b.a(parcel, 7, googleMapOptions.gQ());
        b.a(parcel, 8, googleMapOptions.gR());
        b.a(parcel, 9, googleMapOptions.gS());
        b.a(parcel, 10, googleMapOptions.gT());
        b.a(parcel, 11, googleMapOptions.gU());
        b.D(parcel, p);
    }
}
