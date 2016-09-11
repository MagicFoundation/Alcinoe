package com.google.android.gms.maps.model;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.b;

public class f {
    static void a(MarkerOptions markerOptions, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, markerOptions.getVersionCode());
        b.a(parcel, 2, markerOptions.getPosition(), i, false);
        b.a(parcel, 3, markerOptions.getTitle(), false);
        b.a(parcel, 4, markerOptions.getSnippet(), false);
        b.a(parcel, 5, markerOptions.hf(), false);
        b.a(parcel, 6, markerOptions.getAnchorU());
        b.a(parcel, 7, markerOptions.getAnchorV());
        b.a(parcel, 8, markerOptions.isDraggable());
        b.a(parcel, 9, markerOptions.isVisible());
        b.D(parcel, p);
    }
}
