package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class a implements Creator<Address> {
    static void a(Address address, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, address.getVersionCode());
        b.a(parcel, 2, address.name, false);
        b.a(parcel, 3, address.KB, false);
        b.a(parcel, 4, address.KC, false);
        b.a(parcel, 5, address.KD, false);
        b.a(parcel, 6, address.oQ, false);
        b.a(parcel, 7, address.Yd, false);
        b.a(parcel, 8, address.Ye, false);
        b.a(parcel, 9, address.KI, false);
        b.a(parcel, 10, address.KK, false);
        b.a(parcel, 11, address.KL);
        b.a(parcel, 12, address.KM, false);
        b.D(parcel, p);
    }

    public Address aS(Parcel parcel) {
        int o = com.google.android.gms.common.internal.safeparcel.a.o(parcel);
        int i = 0;
        String str = null;
        String str2 = null;
        String str3 = null;
        String str4 = null;
        String str5 = null;
        String str6 = null;
        String str7 = null;
        String str8 = null;
        String str9 = null;
        boolean z = false;
        String str10 = null;
        while (parcel.dataPosition() < o) {
            int n = com.google.android.gms.common.internal.safeparcel.a.n(parcel);
            switch (com.google.android.gms.common.internal.safeparcel.a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    str2 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    str3 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    str4 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    str5 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    str6 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    str7 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    str8 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    str9 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    z = com.google.android.gms.common.internal.safeparcel.a.c(parcel, n);
                    break;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    str10 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                default:
                    com.google.android.gms.common.internal.safeparcel.a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new Address(i, str, str2, str3, str4, str5, str6, str7, str8, str9, z, str10);
        }
        throw new com.google.android.gms.common.internal.safeparcel.a.a("Overread allowed size end=" + o, parcel);
    }

    public Address[] bY(int i) {
        return new Address[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aS(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bY(x0);
    }
}
