package com.google.android.gms.maps;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.maps.model.CameraPosition;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class GoogleMapOptionsCreator implements Creator<GoogleMapOptions> {
    public static final int CONTENT_DESCRIPTION = 0;

    static void a(GoogleMapOptions googleMapOptions, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, googleMapOptions.getVersionCode());
        b.a(parcel, 2, googleMapOptions.gN());
        b.a(parcel, 3, googleMapOptions.gO());
        b.c(parcel, 4, googleMapOptions.getMapType());
        b.a(parcel, 5, googleMapOptions.getCamera(), i, false);
        b.a(parcel, 6, googleMapOptions.gP());
        b.a(parcel, 7, googleMapOptions.gQ());
        b.a(parcel, 8, googleMapOptions.gR());
        b.a(parcel, 9, googleMapOptions.gS());
        b.a(parcel, 10, googleMapOptions.gT());
        b.a(parcel, 11, googleMapOptions.gU());
        b.D(parcel, p);
    }

    public GoogleMapOptions createFromParcel(Parcel parcel) {
        byte b = (byte) 0;
        int o = a.o(parcel);
        CameraPosition cameraPosition = null;
        byte b2 = (byte) 0;
        byte b3 = (byte) 0;
        byte b4 = (byte) 0;
        byte b5 = (byte) 0;
        byte b6 = (byte) 0;
        int i = 0;
        byte b7 = (byte) 0;
        byte b8 = (byte) 0;
        int i2 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i2 = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    b8 = a.e(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    b7 = a.e(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    cameraPosition = (CameraPosition) a.a(parcel, n, CameraPosition.CREATOR);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    b6 = a.e(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    b5 = a.e(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    b4 = a.e(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    b3 = a.e(parcel, n);
                    break;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    b2 = a.e(parcel, n);
                    break;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    b = a.e(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new GoogleMapOptions(i2, b8, b7, i, cameraPosition, b6, b5, b4, b3, b2, b);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public GoogleMapOptions[] newArray(int size) {
        return new GoogleMapOptions[size];
    }
}
