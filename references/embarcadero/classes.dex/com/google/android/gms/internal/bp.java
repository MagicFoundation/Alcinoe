package com.google.android.gms.internal;

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

public class bp implements Creator<bq> {
    static void a(bq bqVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, bqVar.versionCode);
        b.a(parcel, 2, bqVar.nr, i, false);
        b.a(parcel, 3, bqVar.at(), false);
        b.a(parcel, 4, bqVar.au(), false);
        b.a(parcel, 5, bqVar.av(), false);
        b.a(parcel, 6, bqVar.aw(), false);
        b.a(parcel, 7, bqVar.nw, false);
        b.a(parcel, 8, bqVar.nx);
        b.a(parcel, 9, bqVar.ny, false);
        b.a(parcel, 10, bqVar.ax(), false);
        b.c(parcel, 11, bqVar.orientation);
        b.c(parcel, 12, bqVar.nA);
        b.a(parcel, 13, bqVar.mZ, false);
        b.a(parcel, 14, bqVar.kN, i, false);
        b.D(parcel, p);
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return e(x0);
    }

    public bq e(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        bn bnVar = null;
        IBinder iBinder = null;
        IBinder iBinder2 = null;
        IBinder iBinder3 = null;
        IBinder iBinder4 = null;
        String str = null;
        boolean z = false;
        String str2 = null;
        IBinder iBinder5 = null;
        int i2 = 0;
        int i3 = 0;
        String str3 = null;
        db dbVar = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    bnVar = (bn) a.a(parcel, n, bn.CREATOR);
                    break;
                case DetectedActivity.STILL /*3*/:
                    iBinder = a.n(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    iBinder2 = a.n(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    iBinder3 = a.n(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    iBinder4 = a.n(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    str = a.m(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    z = a.c(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    str2 = a.m(parcel, n);
                    break;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    iBinder5 = a.n(parcel, n);
                    break;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    i2 = a.g(parcel, n);
                    break;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    i3 = a.g(parcel, n);
                    break;
                case CommonStatusCodes.ERROR /*13*/:
                    str3 = a.m(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                    dbVar = (db) a.a(parcel, n, db.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new bq(i, bnVar, iBinder, iBinder2, iBinder3, iBinder4, str, z, str2, iBinder5, i2, i3, str3, dbVar);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public bq[] j(int i) {
        return new bq[i];
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return j(x0);
    }
}
