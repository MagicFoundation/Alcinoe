package com.google.android.gms.maps.model;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.b;

public class c {
    static void a(GroundOverlayOptions groundOverlayOptions, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, groundOverlayOptions.getVersionCode());
        b.a(parcel, 2, groundOverlayOptions.he(), false);
        b.a(parcel, 3, groundOverlayOptions.getLocation(), i, false);
        b.a(parcel, 4, groundOverlayOptions.getWidth());
        b.a(parcel, 5, groundOverlayOptions.getHeight());
        b.a(parcel, 6, groundOverlayOptions.getBounds(), i, false);
        b.a(parcel, 7, groundOverlayOptions.getBearing());
        b.a(parcel, 8, groundOverlayOptions.getZIndex());
        b.a(parcel, 9, groundOverlayOptions.isVisible());
        b.a(parcel, 10, groundOverlayOptions.getTransparency());
        b.a(parcel, 11, groundOverlayOptions.getAnchorU());
        b.a(parcel, 12, groundOverlayOptions.getAnchorV());
        b.D(parcel, p);
    }
}
