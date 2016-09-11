package com.google.android.gms.maps.model;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;
import java.util.List;

public class PolygonOptionsCreator implements Creator<PolygonOptions> {
    public static final int CONTENT_DESCRIPTION = 0;

    static void a(PolygonOptions polygonOptions, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, polygonOptions.getVersionCode());
        b.b(parcel, 2, polygonOptions.getPoints(), false);
        b.c(parcel, 3, polygonOptions.hg(), false);
        b.a(parcel, 4, polygonOptions.getStrokeWidth());
        b.c(parcel, 5, polygonOptions.getStrokeColor());
        b.c(parcel, 6, polygonOptions.getFillColor());
        b.a(parcel, 7, polygonOptions.getZIndex());
        b.a(parcel, 8, polygonOptions.isVisible());
        b.a(parcel, 9, polygonOptions.isGeodesic());
        b.D(parcel, p);
    }

    public PolygonOptions createFromParcel(Parcel parcel) {
        float f = 0.0f;
        boolean z = false;
        int o = a.o(parcel);
        List list = null;
        List arrayList = new ArrayList();
        boolean z2 = false;
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
                    list = a.c(parcel, n, LatLng.CREATOR);
                    break;
                case DetectedActivity.STILL /*3*/:
                    a.a(parcel, n, arrayList, getClass().getClassLoader());
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
                    z2 = a.c(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    z = a.c(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new PolygonOptions(i3, list, arrayList, f2, i2, i, f, z2, z);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public PolygonOptions[] newArray(int size) {
        return new PolygonOptions[size];
    }
}
