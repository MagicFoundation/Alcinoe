package com.google.android.gms.maps.model;

import android.os.IBinder;
import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class TileOverlayOptionsCreator implements Creator<TileOverlayOptions> {
    public static final int CONTENT_DESCRIPTION = 0;

    static void a(TileOverlayOptions tileOverlayOptions, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, tileOverlayOptions.getVersionCode());
        b.a(parcel, 2, tileOverlayOptions.hh(), false);
        b.a(parcel, 3, tileOverlayOptions.isVisible());
        b.a(parcel, 4, tileOverlayOptions.getZIndex());
        b.a(parcel, 5, tileOverlayOptions.getFadeIn());
        b.D(parcel, p);
    }

    public TileOverlayOptions createFromParcel(Parcel parcel) {
        boolean z = false;
        int o = a.o(parcel);
        IBinder iBinder = null;
        float f = 0.0f;
        boolean z2 = true;
        int i = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    iBinder = a.n(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    z = a.c(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    f = a.j(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    z2 = a.c(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new TileOverlayOptions(i, iBinder, z, f, z2);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public TileOverlayOptions[] newArray(int size) {
        return new TileOverlayOptions[size];
    }
}
