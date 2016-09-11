package com.google.android.gms.identity.intents.model;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class b implements Creator<UserAddress> {
    static void a(UserAddress userAddress, Parcel parcel, int i) {
        int p = com.google.android.gms.common.internal.safeparcel.b.p(parcel);
        com.google.android.gms.common.internal.safeparcel.b.c(parcel, 1, userAddress.getVersionCode());
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 2, userAddress.name, false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 3, userAddress.KB, false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 4, userAddress.KC, false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 5, userAddress.KD, false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 6, userAddress.KE, false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 7, userAddress.KF, false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 8, userAddress.KG, false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 9, userAddress.KH, false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 10, userAddress.oQ, false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 11, userAddress.KI, false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 12, userAddress.KJ, false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 13, userAddress.KK, false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 14, userAddress.KL);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 15, userAddress.KM, false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 16, userAddress.KN, false);
        com.google.android.gms.common.internal.safeparcel.b.D(parcel, p);
    }

    public UserAddress at(Parcel parcel) {
        int o = a.o(parcel);
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
        String str10 = null;
        String str11 = null;
        String str12 = null;
        boolean z = false;
        String str13 = null;
        String str14 = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str = a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    str2 = a.m(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    str3 = a.m(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    str4 = a.m(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    str5 = a.m(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    str6 = a.m(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    str7 = a.m(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    str8 = a.m(parcel, n);
                    break;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    str9 = a.m(parcel, n);
                    break;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    str10 = a.m(parcel, n);
                    break;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    str11 = a.m(parcel, n);
                    break;
                case CommonStatusCodes.ERROR /*13*/:
                    str12 = a.m(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                    z = a.c(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                    str13 = a.m(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                    str14 = a.m(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new UserAddress(i, str, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str12, z, str13, str14);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public UserAddress[] bg(int i) {
        return new UserAddress[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return at(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bg(x0);
    }
}
