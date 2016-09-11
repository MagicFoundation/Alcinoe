package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.internal.ir.d;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.HashSet;
import java.util.Set;

public class iy implements Creator<d> {
    static void a(d dVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        Set hB = dVar.hB();
        if (hB.contains(Integer.valueOf(1))) {
            b.c(parcel, 1, dVar.getVersionCode());
        }
        if (hB.contains(Integer.valueOf(2))) {
            b.a(parcel, 2, dVar.getFamilyName(), true);
        }
        if (hB.contains(Integer.valueOf(3))) {
            b.a(parcel, 3, dVar.getFormatted(), true);
        }
        if (hB.contains(Integer.valueOf(4))) {
            b.a(parcel, 4, dVar.getGivenName(), true);
        }
        if (hB.contains(Integer.valueOf(5))) {
            b.a(parcel, 5, dVar.getHonorificPrefix(), true);
        }
        if (hB.contains(Integer.valueOf(6))) {
            b.a(parcel, 6, dVar.getHonorificSuffix(), true);
        }
        if (hB.contains(Integer.valueOf(7))) {
            b.a(parcel, 7, dVar.getMiddleName(), true);
        }
        b.D(parcel, p);
    }

    public d aO(Parcel parcel) {
        String str = null;
        int o = a.o(parcel);
        Set hashSet = new HashSet();
        int i = 0;
        String str2 = null;
        String str3 = null;
        String str4 = null;
        String str5 = null;
        String str6 = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(1));
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str6 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(2));
                    break;
                case DetectedActivity.STILL /*3*/:
                    str5 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(3));
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    str4 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(4));
                    break;
                case DetectedActivity.TILTING /*5*/:
                    str3 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(5));
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    str2 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(6));
                    break;
                case Error.AVS_DECLINE /*7*/:
                    str = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(7));
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new d(hashSet, i, str6, str5, str4, str3, str2, str);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public d[] bL(int i) {
        return new d[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aO(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bL(x0);
    }
}
