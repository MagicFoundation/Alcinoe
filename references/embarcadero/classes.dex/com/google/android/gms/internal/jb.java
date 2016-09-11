package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.internal.ir.h;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.HashSet;
import java.util.Set;

public class jb implements Creator<h> {
    static void a(h hVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        Set hB = hVar.hB();
        if (hB.contains(Integer.valueOf(1))) {
            b.c(parcel, 1, hVar.getVersionCode());
        }
        if (hB.contains(Integer.valueOf(3))) {
            b.c(parcel, 3, hVar.io());
        }
        if (hB.contains(Integer.valueOf(4))) {
            b.a(parcel, 4, hVar.getValue(), true);
        }
        if (hB.contains(Integer.valueOf(5))) {
            b.a(parcel, 5, hVar.getLabel(), true);
        }
        if (hB.contains(Integer.valueOf(6))) {
            b.c(parcel, 6, hVar.getType());
        }
        b.D(parcel, p);
    }

    public h aR(Parcel parcel) {
        String str = null;
        int i = 0;
        int o = a.o(parcel);
        Set hashSet = new HashSet();
        int i2 = 0;
        String str2 = null;
        int i3 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i3 = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(1));
                    break;
                case DetectedActivity.STILL /*3*/:
                    i = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(3));
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    str = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(4));
                    break;
                case DetectedActivity.TILTING /*5*/:
                    str2 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(5));
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    i2 = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(6));
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new h(hashSet, i3, str2, i2, str, i);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public h[] bO(int i) {
        return new h[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aR(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bO(x0);
    }
}
