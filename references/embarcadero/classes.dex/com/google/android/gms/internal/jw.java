package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class jw implements Creator<jv> {
    static void a(jv jvVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, jvVar.getVersionCode());
        b.a(parcel, 2, jvVar.ZK, false);
        b.a(parcel, 3, jvVar.oi, false);
        b.a(parcel, 4, jvVar.ZO, i, false);
        b.a(parcel, 5, jvVar.ZP, i, false);
        b.a(parcel, 6, jvVar.ZQ, i, false);
        b.D(parcel, p);
    }

    public jv bn(Parcel parcel) {
        jt jtVar = null;
        int o = a.o(parcel);
        int i = 0;
        jt jtVar2 = null;
        jr jrVar = null;
        String str = null;
        String str2 = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str2 = a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    str = a.m(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    jrVar = (jr) a.a(parcel, n, jr.CREATOR);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    jtVar2 = (jt) a.a(parcel, n, jt.CREATOR);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    jtVar = (jt) a.a(parcel, n, jt.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new jv(i, str2, str, jrVar, jtVar2, jtVar);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return bn(x0);
    }

    public jv[] ct(int i) {
        return new jv[i];
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return ct(x0);
    }
}
