package com.google.android.gms.internal;

import android.location.Location;
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
import java.util.List;

public class aa implements Creator<z> {
    static void a(z zVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, zVar.versionCode);
        b.a(parcel, 2, zVar.le);
        b.a(parcel, 3, zVar.extras, false);
        b.c(parcel, 4, zVar.lf);
        b.a(parcel, 5, zVar.lg, false);
        b.a(parcel, 6, zVar.lh);
        b.c(parcel, 7, zVar.tagForChildDirectedTreatment);
        b.a(parcel, 8, zVar.li);
        b.a(parcel, 9, zVar.lj, false);
        b.a(parcel, 10, zVar.lk, i, false);
        b.a(parcel, 11, zVar.ll, i, false);
        b.a(parcel, 12, zVar.lm, false);
        b.D(parcel, p);
    }

    public z a(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        long j = 0;
        Bundle bundle = null;
        int i2 = 0;
        List list = null;
        boolean z = false;
        int i3 = 0;
        boolean z2 = false;
        String str = null;
        am amVar = null;
        Location location = null;
        String str2 = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    j = a.h(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    bundle = a.o(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    i2 = a.g(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    list = a.y(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    z = a.c(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    i3 = a.g(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    z2 = a.c(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    str = a.m(parcel, n);
                    break;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    amVar = (am) a.a(parcel, n, am.CREATOR);
                    break;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    location = (Location) a.a(parcel, n, Location.CREATOR);
                    break;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    str2 = a.m(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new z(i, j, bundle, i2, list, z, i3, z2, str, amVar, location, str2);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public z[] b(int i) {
        return new z[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return a(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return b(x0);
    }
}
