package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import android.support.v4.util.TimeUtils;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.internal.ir.c;
import com.google.android.gms.internal.ir.d;
import com.google.android.gms.internal.ir.f;
import com.google.android.gms.internal.ir.g;
import com.google.android.gms.internal.ir.h;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class is implements Creator<ir> {
    static void a(ir irVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        Set hB = irVar.hB();
        if (hB.contains(Integer.valueOf(1))) {
            b.c(parcel, 1, irVar.getVersionCode());
        }
        if (hB.contains(Integer.valueOf(2))) {
            b.a(parcel, 2, irVar.getAboutMe(), true);
        }
        if (hB.contains(Integer.valueOf(3))) {
            b.a(parcel, 3, irVar.hW(), i, true);
        }
        if (hB.contains(Integer.valueOf(4))) {
            b.a(parcel, 4, irVar.getBirthday(), true);
        }
        if (hB.contains(Integer.valueOf(5))) {
            b.a(parcel, 5, irVar.getBraggingRights(), true);
        }
        if (hB.contains(Integer.valueOf(6))) {
            b.c(parcel, 6, irVar.getCircledByCount());
        }
        if (hB.contains(Integer.valueOf(7))) {
            b.a(parcel, 7, irVar.hX(), i, true);
        }
        if (hB.contains(Integer.valueOf(8))) {
            b.a(parcel, 8, irVar.getCurrentLocation(), true);
        }
        if (hB.contains(Integer.valueOf(9))) {
            b.a(parcel, 9, irVar.getDisplayName(), true);
        }
        if (hB.contains(Integer.valueOf(12))) {
            b.c(parcel, 12, irVar.getGender());
        }
        if (hB.contains(Integer.valueOf(14))) {
            b.a(parcel, 14, irVar.getId(), true);
        }
        if (hB.contains(Integer.valueOf(15))) {
            b.a(parcel, 15, irVar.hY(), i, true);
        }
        if (hB.contains(Integer.valueOf(16))) {
            b.a(parcel, 16, irVar.isPlusUser());
        }
        if (hB.contains(Integer.valueOf(19))) {
            b.a(parcel, 19, irVar.hZ(), i, true);
        }
        if (hB.contains(Integer.valueOf(18))) {
            b.a(parcel, 18, irVar.getLanguage(), true);
        }
        if (hB.contains(Integer.valueOf(21))) {
            b.c(parcel, 21, irVar.getObjectType());
        }
        if (hB.contains(Integer.valueOf(20))) {
            b.a(parcel, 20, irVar.getNickname(), true);
        }
        if (hB.contains(Integer.valueOf(23))) {
            b.b(parcel, 23, irVar.ib(), true);
        }
        if (hB.contains(Integer.valueOf(22))) {
            b.b(parcel, 22, irVar.ia(), true);
        }
        if (hB.contains(Integer.valueOf(25))) {
            b.c(parcel, 25, irVar.getRelationshipStatus());
        }
        if (hB.contains(Integer.valueOf(24))) {
            b.c(parcel, 24, irVar.getPlusOneCount());
        }
        if (hB.contains(Integer.valueOf(27))) {
            b.a(parcel, 27, irVar.getUrl(), true);
        }
        if (hB.contains(Integer.valueOf(26))) {
            b.a(parcel, 26, irVar.getTagline(), true);
        }
        if (hB.contains(Integer.valueOf(29))) {
            b.a(parcel, 29, irVar.isVerified());
        }
        if (hB.contains(Integer.valueOf(28))) {
            b.b(parcel, 28, irVar.ic(), true);
        }
        b.D(parcel, p);
    }

    public ir aI(Parcel parcel) {
        int o = a.o(parcel);
        Set hashSet = new HashSet();
        int i = 0;
        String str = null;
        ir.a aVar = null;
        String str2 = null;
        String str3 = null;
        int i2 = 0;
        ir.b bVar = null;
        String str4 = null;
        String str5 = null;
        int i3 = 0;
        String str6 = null;
        c cVar = null;
        boolean z = false;
        String str7 = null;
        d dVar = null;
        String str8 = null;
        int i4 = 0;
        List list = null;
        List list2 = null;
        int i5 = 0;
        int i6 = 0;
        String str9 = null;
        String str10 = null;
        List list3 = null;
        boolean z2 = false;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(1));
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(2));
                    break;
                case DetectedActivity.STILL /*3*/:
                    ir.a aVar2 = (ir.a) a.a(parcel, n, ir.a.CREATOR);
                    hashSet.add(Integer.valueOf(3));
                    aVar = aVar2;
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    str2 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(4));
                    break;
                case DetectedActivity.TILTING /*5*/:
                    str3 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(5));
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    i2 = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(6));
                    break;
                case Error.AVS_DECLINE /*7*/:
                    ir.b bVar2 = (ir.b) a.a(parcel, n, ir.b.CREATOR);
                    hashSet.add(Integer.valueOf(7));
                    bVar = bVar2;
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    str4 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(8));
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    str5 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(9));
                    break;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    i3 = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(12));
                    break;
                case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                    str6 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(14));
                    break;
                case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                    c cVar2 = (c) a.a(parcel, n, c.CREATOR);
                    hashSet.add(Integer.valueOf(15));
                    cVar = cVar2;
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                    z = a.c(parcel, n);
                    hashSet.add(Integer.valueOf(16));
                    break;
                case 18:
                    str7 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(18));
                    break;
                case TimeUtils.HUNDRED_DAY_FIELD_LEN /*19*/:
                    d dVar2 = (d) a.a(parcel, n, d.CREATOR);
                    hashSet.add(Integer.valueOf(19));
                    dVar = dVar2;
                    break;
                case 20:
                    str8 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(20));
                    break;
                case 21:
                    i4 = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(21));
                    break;
                case 22:
                    list = a.c(parcel, n, f.CREATOR);
                    hashSet.add(Integer.valueOf(22));
                    break;
                case 23:
                    list2 = a.c(parcel, n, g.CREATOR);
                    hashSet.add(Integer.valueOf(23));
                    break;
                case 24:
                    i5 = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(24));
                    break;
                case 25:
                    i6 = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(25));
                    break;
                case 26:
                    str9 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(26));
                    break;
                case 27:
                    str10 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(27));
                    break;
                case 28:
                    list3 = a.c(parcel, n, h.CREATOR);
                    hashSet.add(Integer.valueOf(28));
                    break;
                case 29:
                    z2 = a.c(parcel, n);
                    hashSet.add(Integer.valueOf(29));
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new ir(hashSet, i, str, aVar, str2, str3, i2, bVar, str4, str5, i3, str6, cVar, z, str7, dVar, str8, i4, list, list2, i5, i6, str9, str10, list3, z2);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public ir[] bF(int i) {
        return new ir[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aI(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bF(x0);
    }
}
