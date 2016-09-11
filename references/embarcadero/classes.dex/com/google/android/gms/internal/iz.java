package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.internal.ir.f;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.HashSet;
import java.util.Set;

public class iz implements Creator<f> {
    static void a(f fVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        Set hB = fVar.hB();
        if (hB.contains(Integer.valueOf(1))) {
            b.c(parcel, 1, fVar.getVersionCode());
        }
        if (hB.contains(Integer.valueOf(2))) {
            b.a(parcel, 2, fVar.getDepartment(), true);
        }
        if (hB.contains(Integer.valueOf(3))) {
            b.a(parcel, 3, fVar.getDescription(), true);
        }
        if (hB.contains(Integer.valueOf(4))) {
            b.a(parcel, 4, fVar.getEndDate(), true);
        }
        if (hB.contains(Integer.valueOf(5))) {
            b.a(parcel, 5, fVar.getLocation(), true);
        }
        if (hB.contains(Integer.valueOf(6))) {
            b.a(parcel, 6, fVar.getName(), true);
        }
        if (hB.contains(Integer.valueOf(7))) {
            b.a(parcel, 7, fVar.isPrimary());
        }
        if (hB.contains(Integer.valueOf(8))) {
            b.a(parcel, 8, fVar.getStartDate(), true);
        }
        if (hB.contains(Integer.valueOf(9))) {
            b.a(parcel, 9, fVar.getTitle(), true);
        }
        if (hB.contains(Integer.valueOf(10))) {
            b.c(parcel, 10, fVar.getType());
        }
        b.D(parcel, p);
    }

    public f aP(Parcel parcel) {
        int i = 0;
        String str = null;
        int o = a.o(parcel);
        Set hashSet = new HashSet();
        String str2 = null;
        boolean z = false;
        String str3 = null;
        String str4 = null;
        String str5 = null;
        String str6 = null;
        String str7 = null;
        int i2 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i2 = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(1));
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str7 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(2));
                    break;
                case DetectedActivity.STILL /*3*/:
                    str6 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(3));
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    str5 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(4));
                    break;
                case DetectedActivity.TILTING /*5*/:
                    str4 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(5));
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    str3 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(6));
                    break;
                case Error.AVS_DECLINE /*7*/:
                    z = a.c(parcel, n);
                    hashSet.add(Integer.valueOf(7));
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    str2 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(8));
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    str = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(9));
                    break;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    i = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(10));
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new f(hashSet, i2, str7, str6, str5, str4, str3, z, str2, str, i);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public f[] bM(int i) {
        return new f[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aP(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bM(x0);
    }
}
