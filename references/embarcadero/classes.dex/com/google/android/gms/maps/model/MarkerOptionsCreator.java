package com.google.android.gms.maps.model;

import android.os.IBinder;
import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class MarkerOptionsCreator implements Creator<MarkerOptions> {
    public static final int CONTENT_DESCRIPTION = 0;

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
        b.a(parcel, 10, markerOptions.isFlat());
        b.a(parcel, 11, markerOptions.getRotation());
        b.a(parcel, 12, markerOptions.getInfoWindowAnchorU());
        b.a(parcel, 13, markerOptions.getInfoWindowAnchorV());
        b.a(parcel, 14, markerOptions.getAlpha());
        b.D(parcel, p);
    }

    public MarkerOptions createFromParcel(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        LatLng latLng = null;
        String str = null;
        String str2 = null;
        IBinder iBinder = null;
        float f = 0.0f;
        float f2 = 0.0f;
        boolean z = false;
        boolean z2 = false;
        boolean z3 = false;
        float f3 = 0.0f;
        float f4 = 0.5f;
        float f5 = 0.0f;
        float f6 = 1.0f;
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
                    str = a.m(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    str2 = a.m(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    iBinder = a.n(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    f = a.j(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    f2 = a.j(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    z = a.c(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    z2 = a.c(parcel, n);
                    break;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    z3 = a.c(parcel, n);
                    break;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    f3 = a.j(parcel, n);
                    break;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    f4 = a.j(parcel, n);
                    break;
                case CommonStatusCodes.ERROR /*13*/:
                    f5 = a.j(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                    f6 = a.j(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new MarkerOptions(i, latLng, str, str2, iBinder, f, f2, z, z2, z3, f3, f4, f5, f6);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public MarkerOptions[] newArray(int size) {
        return new MarkerOptions[size];
    }
}
