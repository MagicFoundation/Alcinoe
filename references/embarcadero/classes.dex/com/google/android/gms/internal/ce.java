package com.google.android.gms.internal;

import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.os.Bundle;
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

public class ce implements Creator<cd> {
    static void a(cd cdVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, cdVar.versionCode);
        b.a(parcel, 2, cdVar.ob, false);
        b.a(parcel, 3, cdVar.oc, i, false);
        b.a(parcel, 4, cdVar.kQ, i, false);
        b.a(parcel, 5, cdVar.adUnitId, false);
        b.a(parcel, 6, cdVar.applicationInfo, i, false);
        b.a(parcel, 7, cdVar.od, i, false);
        b.a(parcel, 8, cdVar.oe, false);
        b.a(parcel, 9, cdVar.of, false);
        b.a(parcel, 10, cdVar.og, false);
        b.a(parcel, 11, cdVar.kN, i, false);
        b.a(parcel, 12, cdVar.oh, false);
        b.D(parcel, p);
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return f(x0);
    }

    public cd f(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        Bundle bundle = null;
        z zVar = null;
        ab abVar = null;
        String str = null;
        ApplicationInfo applicationInfo = null;
        PackageInfo packageInfo = null;
        String str2 = null;
        String str3 = null;
        String str4 = null;
        db dbVar = null;
        Bundle bundle2 = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    bundle = a.o(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    zVar = (z) a.a(parcel, n, z.CREATOR);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    abVar = (ab) a.a(parcel, n, ab.CREATOR);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    str = a.m(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    applicationInfo = (ApplicationInfo) a.a(parcel, n, ApplicationInfo.CREATOR);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    packageInfo = (PackageInfo) a.a(parcel, n, PackageInfo.CREATOR);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    str2 = a.m(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    str3 = a.m(parcel, n);
                    break;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    str4 = a.m(parcel, n);
                    break;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    dbVar = (db) a.a(parcel, n, db.CREATOR);
                    break;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    bundle2 = a.o(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new cd(i, bundle, zVar, abVar, str, applicationInfo, packageInfo, str2, str3, str4, dbVar, bundle2);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public cd[] k(int i) {
        return new cd[i];
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return k(x0);
    }
}
