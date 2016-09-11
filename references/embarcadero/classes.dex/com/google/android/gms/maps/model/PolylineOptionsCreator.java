package com.google.android.gms.maps.model;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.List;

public class PolylineOptionsCreator implements Creator<PolylineOptions> {
    public static final int CONTENT_DESCRIPTION = 0;

    static void a(PolylineOptions polylineOptions, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, polylineOptions.getVersionCode());
        b.b(parcel, 2, polylineOptions.getPoints(), false);
        b.a(parcel, 3, polylineOptions.getWidth());
        b.c(parcel, 4, polylineOptions.getColor());
        b.a(parcel, 5, polylineOptions.getZIndex());
        b.a(parcel, 6, polylineOptions.isVisible());
        b.a(parcel, 7, polylineOptions.isGeodesic());
        b.D(parcel, p);
    }

    public PolylineOptions createFromParcel(Parcel parcel) {
        float f = 0.0f;
        boolean z = false;
        int o = a.o(parcel);
        List list = null;
        boolean z2 = false;
        int i = 0;
        float f2 = 0.0f;
        int i2 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i2 = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    list = a.c(parcel, n, LatLng.CREATOR);
                    break;
                case DetectedActivity.STILL /*3*/:
                    f2 = a.j(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    f = a.j(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    z2 = a.c(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    z = a.c(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new PolylineOptions(i2, list, f2, i, f, z2, z);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public PolylineOptions[] newArray(int size) {
        return new PolylineOptions[size];
    }
}
