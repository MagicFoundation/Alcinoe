package com.google.android.gms.maps.model;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class CameraPositionCreator implements Creator<CameraPosition> {
    public static final int CONTENT_DESCRIPTION = 0;

    static void a(CameraPosition cameraPosition, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, cameraPosition.getVersionCode());
        b.a(parcel, 2, cameraPosition.target, i, false);
        b.a(parcel, 3, cameraPosition.zoom);
        b.a(parcel, 4, cameraPosition.tilt);
        b.a(parcel, 5, cameraPosition.bearing);
        b.D(parcel, p);
    }

    public CameraPosition createFromParcel(Parcel parcel) {
        float f = 0.0f;
        int o = a.o(parcel);
        int i = 0;
        LatLng latLng = null;
        float f2 = 0.0f;
        float f3 = 0.0f;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    latLng = (LatLng) a.a(parcel, n, LatLng.CREATOR);
                    break;
                case DetectedActivity.STILL /*3*/:
                    f3 = a.j(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    f2 = a.j(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    f = a.j(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new CameraPosition(i, latLng, f3, f2, f);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public CameraPosition[] newArray(int size) {
        return new CameraPosition[size];
    }
}
