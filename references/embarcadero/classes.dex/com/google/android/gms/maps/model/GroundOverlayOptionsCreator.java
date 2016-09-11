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

public class GroundOverlayOptionsCreator implements Creator<GroundOverlayOptions> {
    public static final int CONTENT_DESCRIPTION = 0;

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

    public GroundOverlayOptions createFromParcel(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        IBinder iBinder = null;
        LatLng latLng = null;
        float f = 0.0f;
        float f2 = 0.0f;
        LatLngBounds latLngBounds = null;
        float f3 = 0.0f;
        float f4 = 0.0f;
        boolean z = false;
        float f5 = 0.0f;
        float f6 = 0.0f;
        float f7 = 0.0f;
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
                    latLng = (LatLng) a.a(parcel, n, LatLng.CREATOR);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    f = a.j(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    f2 = a.j(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    latLngBounds = (LatLngBounds) a.a(parcel, n, LatLngBounds.CREATOR);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    f3 = a.j(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    f4 = a.j(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    z = a.c(parcel, n);
                    break;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    f5 = a.j(parcel, n);
                    break;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    f6 = a.j(parcel, n);
                    break;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    f7 = a.j(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new GroundOverlayOptions(i, iBinder, latLng, f, f2, latLngBounds, f3, f4, z, f5, f6, f7);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public GroundOverlayOptions[] newArray(int size) {
        return new GroundOverlayOptions[size];
    }
}
