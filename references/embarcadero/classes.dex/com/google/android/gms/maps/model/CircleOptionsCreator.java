package com.google.android.gms.maps.model;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class CircleOptionsCreator implements Creator<CircleOptions> {
    public static final int CONTENT_DESCRIPTION = 0;

    static void a(CircleOptions circleOptions, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, circleOptions.getVersionCode());
        b.a(parcel, 2, circleOptions.getCenter(), i, false);
        b.a(parcel, 3, circleOptions.getRadius());
        b.a(parcel, 4, circleOptions.getStrokeWidth());
        b.c(parcel, 5, circleOptions.getStrokeColor());
        b.c(parcel, 6, circleOptions.getFillColor());
        b.a(parcel, 7, circleOptions.getZIndex());
        b.a(parcel, 8, circleOptions.isVisible());
        b.D(parcel, p);
    }

    public CircleOptions createFromParcel(Parcel parcel) {
        float f = 0.0f;
        boolean z = false;
        int o = a.o(parcel);
        LatLng latLng = null;
        double d = 0.0d;
        int i = 0;
        int i2 = 0;
        float f2 = 0.0f;
        int i3 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i3 = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    latLng = (LatLng) a.a(parcel, n, LatLng.CREATOR);
                    break;
                case DetectedActivity.STILL /*3*/:
                    d = a.k(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    f2 = a.j(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    i2 = a.g(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    i = a.g(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    f = a.j(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    z = a.c(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new CircleOptions(i3, latLng, d, f2, i2, i, f, z);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public CircleOptions[] newArray(int size) {
        return new CircleOptions[size];
    }
}
