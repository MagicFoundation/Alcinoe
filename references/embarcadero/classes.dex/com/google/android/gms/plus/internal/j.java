package com.google.android.gms.plus.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class j implements Creator<h> {
    static void a(h hVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.a(parcel, 1, hVar.getAccountName(), false);
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, hVar.getVersionCode());
        b.a(parcel, 2, hVar.hq(), false);
        b.a(parcel, 3, hVar.hr(), false);
        b.a(parcel, 4, hVar.hs(), false);
        b.a(parcel, 5, hVar.ht(), false);
        b.a(parcel, 6, hVar.hu(), false);
        b.a(parcel, 7, hVar.hv(), false);
        b.a(parcel, 8, hVar.hw(), false);
        b.a(parcel, 9, hVar.hx(), i, false);
        b.D(parcel, p);
    }

    public h aF(Parcel parcel) {
        PlusCommonExtras plusCommonExtras = null;
        int o = a.o(parcel);
        int i = 0;
        String str = null;
        String str2 = null;
        String str3 = null;
        String str4 = null;
        String[] strArr = null;
        String[] strArr2 = null;
        String[] strArr3 = null;
        String str5 = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    str5 = a.m(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    strArr3 = a.x(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    strArr2 = a.x(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    strArr = a.x(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    str4 = a.m(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    str3 = a.m(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    str2 = a.m(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    str = a.m(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    plusCommonExtras = (PlusCommonExtras) a.a(parcel, n, PlusCommonExtras.CREATOR);
                    break;
                case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE /*1000*/:
                    i = a.g(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new h(i, str5, strArr3, strArr2, strArr, str4, str3, str2, str, plusCommonExtras);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public h[] bC(int i) {
        return new h[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aF(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bC(x0);
    }
}
