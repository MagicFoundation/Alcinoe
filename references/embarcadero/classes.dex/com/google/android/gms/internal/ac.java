package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class ac implements Creator<ab> {
    static void a(ab abVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, abVar.versionCode);
        b.a(parcel, 2, abVar.ln, false);
        b.c(parcel, 3, abVar.height);
        b.c(parcel, 4, abVar.heightPixels);
        b.a(parcel, 5, abVar.lo);
        b.c(parcel, 6, abVar.width);
        b.c(parcel, 7, abVar.widthPixels);
        b.a(parcel, 8, abVar.lp, i, false);
        b.D(parcel, p);
    }

    public ab b(Parcel parcel) {
        ab[] abVarArr = null;
        int i = 0;
        int o = a.o(parcel);
        int i2 = 0;
        boolean z = false;
        int i3 = 0;
        int i4 = 0;
        String str = null;
        int i5 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i5 = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str = a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    i4 = a.g(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    i3 = a.g(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    z = a.c(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    i2 = a.g(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    i = a.g(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    abVarArr = (ab[]) a.b(parcel, n, ab.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new ab(i5, str, i4, i3, z, i2, i, abVarArr);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public ab[] c(int i) {
        return new ab[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return b(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return c(x0);
    }
}
