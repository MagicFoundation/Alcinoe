package com.google.android.gms.internal;

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

public class an implements Creator<am> {
    static void a(am amVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, amVar.versionCode);
        b.c(parcel, 2, amVar.lI);
        b.c(parcel, 3, amVar.backgroundColor);
        b.c(parcel, 4, amVar.lJ);
        b.c(parcel, 5, amVar.lK);
        b.c(parcel, 6, amVar.lL);
        b.c(parcel, 7, amVar.lM);
        b.c(parcel, 8, amVar.lN);
        b.c(parcel, 9, amVar.lO);
        b.a(parcel, 10, amVar.lP, false);
        b.c(parcel, 11, amVar.lQ);
        b.a(parcel, 12, amVar.lR, false);
        b.c(parcel, 13, amVar.lS);
        b.c(parcel, 14, amVar.lT);
        b.a(parcel, 15, amVar.lU, false);
        b.D(parcel, p);
    }

    public am c(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        int i2 = 0;
        int i3 = 0;
        int i4 = 0;
        int i5 = 0;
        int i6 = 0;
        int i7 = 0;
        int i8 = 0;
        int i9 = 0;
        String str = null;
        int i10 = 0;
        String str2 = null;
        int i11 = 0;
        int i12 = 0;
        String str3 = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    i2 = a.g(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    i3 = a.g(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    i4 = a.g(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    i5 = a.g(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    i6 = a.g(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    i7 = a.g(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    i8 = a.g(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    i9 = a.g(parcel, n);
                    break;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    str = a.m(parcel, n);
                    break;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    i10 = a.g(parcel, n);
                    break;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    str2 = a.m(parcel, n);
                    break;
                case CommonStatusCodes.ERROR /*13*/:
                    i11 = a.g(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                    i12 = a.g(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                    str3 = a.m(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new am(i, i2, i3, i4, i5, i6, i7, i8, i9, str, i10, str2, i11, i12, str3);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return c(x0);
    }

    public am[] e(int i) {
        return new am[i];
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return e(x0);
    }
}
