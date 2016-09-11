package com.google.android.gms.maps.model;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class VisibleRegionCreator implements Creator<VisibleRegion> {
    public static final int CONTENT_DESCRIPTION = 0;

    static void a(VisibleRegion visibleRegion, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, visibleRegion.getVersionCode());
        b.a(parcel, 2, visibleRegion.nearLeft, i, false);
        b.a(parcel, 3, visibleRegion.nearRight, i, false);
        b.a(parcel, 4, visibleRegion.farLeft, i, false);
        b.a(parcel, 5, visibleRegion.farRight, i, false);
        b.a(parcel, 6, visibleRegion.latLngBounds, i, false);
        b.D(parcel, p);
    }

    public VisibleRegion createFromParcel(Parcel parcel) {
        LatLngBounds latLngBounds = null;
        int o = a.o(parcel);
        int i = 0;
        LatLng latLng = null;
        LatLng latLng2 = null;
        LatLng latLng3 = null;
        LatLng latLng4 = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    latLng4 = (LatLng) a.a(parcel, n, LatLng.CREATOR);
                    break;
                case DetectedActivity.STILL /*3*/:
                    latLng3 = (LatLng) a.a(parcel, n, LatLng.CREATOR);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    latLng2 = (LatLng) a.a(parcel, n, LatLng.CREATOR);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    latLng = (LatLng) a.a(parcel, n, LatLng.CREATOR);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    latLngBounds = (LatLngBounds) a.a(parcel, n, LatLngBounds.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new VisibleRegion(i, latLng4, latLng3, latLng2, latLng, latLngBounds);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public VisibleRegion[] newArray(int size) {
        return new VisibleRegion[size];
    }
}
