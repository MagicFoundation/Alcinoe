package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.internal.fb.a;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class fc implements Creator<a> {
    static void a(a aVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, aVar.getVersionCode());
        b.c(parcel, 2, aVar.el());
        b.a(parcel, 3, aVar.er());
        b.c(parcel, 4, aVar.em());
        b.a(parcel, 5, aVar.es());
        b.a(parcel, 6, aVar.et(), false);
        b.c(parcel, 7, aVar.eu());
        b.a(parcel, 8, aVar.ew(), false);
        b.a(parcel, 9, aVar.ey(), i, false);
        b.D(parcel, p);
    }

    public a[] W(int i) {
        return new a[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return t(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return W(x0);
    }

    public a t(Parcel parcel) {
        ew ewVar = null;
        int i = 0;
        int o = com.google.android.gms.common.internal.safeparcel.a.o(parcel);
        String str = null;
        String str2 = null;
        boolean z = false;
        int i2 = 0;
        boolean z2 = false;
        int i3 = 0;
        int i4 = 0;
        while (parcel.dataPosition() < o) {
            int n = com.google.android.gms.common.internal.safeparcel.a.n(parcel);
            switch (com.google.android.gms.common.internal.safeparcel.a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i4 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    i3 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    z2 = com.google.android.gms.common.internal.safeparcel.a.c(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    i2 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    z = com.google.android.gms.common.internal.safeparcel.a.c(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    str2 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    i = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    str = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    ewVar = (ew) com.google.android.gms.common.internal.safeparcel.a.a(parcel, n, ew.CREATOR);
                    break;
                default:
                    com.google.android.gms.common.internal.safeparcel.a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new a(i4, i3, z2, i2, z, str2, i, str, ewVar);
        }
        throw new com.google.android.gms.common.internal.safeparcel.a.a("Overread allowed size end=" + o, parcel);
    }
}
