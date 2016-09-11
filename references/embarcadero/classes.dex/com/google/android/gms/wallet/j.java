package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import android.support.v4.util.TimeUtils;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.internal.fj;
import com.google.android.gms.internal.jj;
import com.google.android.gms.internal.jl;
import com.google.android.gms.internal.jp;
import com.google.android.gms.internal.jr;
import com.google.android.gms.internal.jt;
import com.google.android.gms.internal.jv;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;

public class j implements Creator<LoyaltyWalletObject> {
    static void a(LoyaltyWalletObject loyaltyWalletObject, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, loyaltyWalletObject.getVersionCode());
        b.a(parcel, 2, loyaltyWalletObject.eN, false);
        b.a(parcel, 3, loyaltyWalletObject.YC, false);
        b.a(parcel, 4, loyaltyWalletObject.YD, false);
        b.a(parcel, 5, loyaltyWalletObject.YE, false);
        b.a(parcel, 6, loyaltyWalletObject.YF, false);
        b.a(parcel, 7, loyaltyWalletObject.YG, false);
        b.a(parcel, 8, loyaltyWalletObject.YH, false);
        b.a(parcel, 9, loyaltyWalletObject.YI, false);
        b.a(parcel, 10, loyaltyWalletObject.YJ, false);
        b.a(parcel, 11, loyaltyWalletObject.YK, false);
        b.c(parcel, 12, loyaltyWalletObject.state);
        b.b(parcel, 13, loyaltyWalletObject.YL, false);
        b.a(parcel, 14, loyaltyWalletObject.YM, i, false);
        b.b(parcel, 15, loyaltyWalletObject.YN, false);
        b.a(parcel, 17, loyaltyWalletObject.YP, false);
        b.a(parcel, 16, loyaltyWalletObject.YO, false);
        b.a(parcel, 19, loyaltyWalletObject.YR);
        b.b(parcel, 18, loyaltyWalletObject.YQ, false);
        b.b(parcel, 21, loyaltyWalletObject.YT, false);
        b.b(parcel, 20, loyaltyWalletObject.YS, false);
        b.a(parcel, 23, loyaltyWalletObject.YV, i, false);
        b.b(parcel, 22, loyaltyWalletObject.YU, false);
        b.D(parcel, p);
    }

    public LoyaltyWalletObject ba(Parcel parcel) {
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
        int i2 = 0;
        ArrayList eH = fj.eH();
        jr jrVar = null;
        ArrayList eH2 = fj.eH();
        String str11 = null;
        String str12 = null;
        ArrayList eH3 = fj.eH();
        boolean z = false;
        ArrayList eH4 = fj.eH();
        ArrayList eH5 = fj.eH();
        ArrayList eH6 = fj.eH();
        jl jlVar = null;
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
                    i2 = a.g(parcel, n);
                    break;
                case CommonStatusCodes.ERROR /*13*/:
                    eH = a.c(parcel, n, jv.CREATOR);
                    break;
                case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                    jrVar = (jr) a.a(parcel, n, jr.CREATOR);
                    break;
                case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                    eH2 = a.c(parcel, n, LatLng.CREATOR);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                    str11 = a.m(parcel, n);
                    break;
                case 17:
                    str12 = a.m(parcel, n);
                    break;
                case 18:
                    eH3 = a.c(parcel, n, jj.CREATOR);
                    break;
                case TimeUtils.HUNDRED_DAY_FIELD_LEN /*19*/:
                    z = a.c(parcel, n);
                    break;
                case 20:
                    eH4 = a.c(parcel, n, jt.CREATOR);
                    break;
                case 21:
                    eH5 = a.c(parcel, n, jp.CREATOR);
                    break;
                case 22:
                    eH6 = a.c(parcel, n, jt.CREATOR);
                    break;
                case 23:
                    jlVar = (jl) a.a(parcel, n, jl.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new LoyaltyWalletObject(i, str, str2, str3, str4, str5, str6, str7, str8, str9, str10, i2, eH, jrVar, eH2, str11, str12, eH3, z, eH4, eH5, eH6, jlVar);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public LoyaltyWalletObject[] cg(int i) {
        return new LoyaltyWalletObject[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return ba(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return cg(x0);
    }
}
