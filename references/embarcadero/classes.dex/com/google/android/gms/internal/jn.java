package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class jn implements Creator<jm> {
    static void a(jm jmVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, jmVar.getVersionCode());
        b.c(parcel, 2, jmVar.ZE);
        b.a(parcel, 3, jmVar.ZF, false);
        b.a(parcel, 4, jmVar.ZG);
        b.a(parcel, 5, jmVar.ZH, false);
        b.a(parcel, 6, jmVar.ZI);
        b.c(parcel, 7, jmVar.ZJ);
        b.D(parcel, p);
    }

    public jm bi(Parcel parcel) {
        String str = null;
        int i = 0;
        int o = a.o(parcel);
        double d = 0.0d;
        long j = 0;
        int i2 = -1;
        String str2 = null;
        int i3 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i3 = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    str2 = a.m(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    d = a.k(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    str = a.m(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    j = a.h(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    i2 = a.g(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new jm(i3, i, str2, d, str, j, i2);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public jm[] co(int i) {
        return new jm[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return bi(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return co(x0);
    }
}
