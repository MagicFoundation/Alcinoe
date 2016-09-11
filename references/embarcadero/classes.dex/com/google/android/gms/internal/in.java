package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import android.support.v4.util.TimeUtils;
import com.google.ads.AdSize;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class in implements Creator<im> {
    static void a(im imVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        Set hB = imVar.hB();
        if (hB.contains(Integer.valueOf(1))) {
            b.c(parcel, 1, imVar.getVersionCode());
        }
        if (hB.contains(Integer.valueOf(2))) {
            b.a(parcel, 2, imVar.hC(), i, true);
        }
        if (hB.contains(Integer.valueOf(3))) {
            b.a(parcel, 3, imVar.getAdditionalName(), true);
        }
        if (hB.contains(Integer.valueOf(4))) {
            b.a(parcel, 4, imVar.hD(), i, true);
        }
        if (hB.contains(Integer.valueOf(5))) {
            b.a(parcel, 5, imVar.getAddressCountry(), true);
        }
        if (hB.contains(Integer.valueOf(6))) {
            b.a(parcel, 6, imVar.getAddressLocality(), true);
        }
        if (hB.contains(Integer.valueOf(7))) {
            b.a(parcel, 7, imVar.getAddressRegion(), true);
        }
        if (hB.contains(Integer.valueOf(8))) {
            b.b(parcel, 8, imVar.hE(), true);
        }
        if (hB.contains(Integer.valueOf(9))) {
            b.c(parcel, 9, imVar.getAttendeeCount());
        }
        if (hB.contains(Integer.valueOf(10))) {
            b.b(parcel, 10, imVar.hF(), true);
        }
        if (hB.contains(Integer.valueOf(11))) {
            b.a(parcel, 11, imVar.hG(), i, true);
        }
        if (hB.contains(Integer.valueOf(12))) {
            b.b(parcel, 12, imVar.hH(), true);
        }
        if (hB.contains(Integer.valueOf(13))) {
            b.a(parcel, 13, imVar.getBestRating(), true);
        }
        if (hB.contains(Integer.valueOf(14))) {
            b.a(parcel, 14, imVar.getBirthDate(), true);
        }
        if (hB.contains(Integer.valueOf(15))) {
            b.a(parcel, 15, imVar.hI(), i, true);
        }
        if (hB.contains(Integer.valueOf(17))) {
            b.a(parcel, 17, imVar.getContentSize(), true);
        }
        if (hB.contains(Integer.valueOf(16))) {
            b.a(parcel, 16, imVar.getCaption(), true);
        }
        if (hB.contains(Integer.valueOf(19))) {
            b.b(parcel, 19, imVar.hJ(), true);
        }
        if (hB.contains(Integer.valueOf(18))) {
            b.a(parcel, 18, imVar.getContentUrl(), true);
        }
        if (hB.contains(Integer.valueOf(21))) {
            b.a(parcel, 21, imVar.getDateModified(), true);
        }
        if (hB.contains(Integer.valueOf(20))) {
            b.a(parcel, 20, imVar.getDateCreated(), true);
        }
        if (hB.contains(Integer.valueOf(23))) {
            b.a(parcel, 23, imVar.getDescription(), true);
        }
        if (hB.contains(Integer.valueOf(22))) {
            b.a(parcel, 22, imVar.getDatePublished(), true);
        }
        if (hB.contains(Integer.valueOf(25))) {
            b.a(parcel, 25, imVar.getEmbedUrl(), true);
        }
        if (hB.contains(Integer.valueOf(24))) {
            b.a(parcel, 24, imVar.getDuration(), true);
        }
        if (hB.contains(Integer.valueOf(27))) {
            b.a(parcel, 27, imVar.getFamilyName(), true);
        }
        if (hB.contains(Integer.valueOf(26))) {
            b.a(parcel, 26, imVar.getEndDate(), true);
        }
        if (hB.contains(Integer.valueOf(29))) {
            b.a(parcel, 29, imVar.hK(), i, true);
        }
        if (hB.contains(Integer.valueOf(28))) {
            b.a(parcel, 28, imVar.getGender(), true);
        }
        if (hB.contains(Integer.valueOf(31))) {
            b.a(parcel, 31, imVar.getHeight(), true);
        }
        if (hB.contains(Integer.valueOf(30))) {
            b.a(parcel, 30, imVar.getGivenName(), true);
        }
        if (hB.contains(Integer.valueOf(34))) {
            b.a(parcel, 34, imVar.hL(), i, true);
        }
        if (hB.contains(Integer.valueOf(32))) {
            b.a(parcel, 32, imVar.getId(), true);
        }
        if (hB.contains(Integer.valueOf(33))) {
            b.a(parcel, 33, imVar.getImage(), true);
        }
        if (hB.contains(Integer.valueOf(38))) {
            b.a(parcel, 38, imVar.getLongitude());
        }
        if (hB.contains(Integer.valueOf(39))) {
            b.a(parcel, 39, imVar.getName(), true);
        }
        if (hB.contains(Integer.valueOf(36))) {
            b.a(parcel, 36, imVar.getLatitude());
        }
        if (hB.contains(Integer.valueOf(37))) {
            b.a(parcel, 37, imVar.hM(), i, true);
        }
        if (hB.contains(Integer.valueOf(42))) {
            b.a(parcel, 42, imVar.getPlayerType(), true);
        }
        if (hB.contains(Integer.valueOf(43))) {
            b.a(parcel, 43, imVar.getPostOfficeBoxNumber(), true);
        }
        if (hB.contains(Integer.valueOf(40))) {
            b.a(parcel, 40, imVar.hN(), i, true);
        }
        if (hB.contains(Integer.valueOf(41))) {
            b.b(parcel, 41, imVar.hO(), true);
        }
        if (hB.contains(Integer.valueOf(46))) {
            b.a(parcel, 46, imVar.hP(), i, true);
        }
        if (hB.contains(Integer.valueOf(47))) {
            b.a(parcel, 47, imVar.getStartDate(), true);
        }
        if (hB.contains(Integer.valueOf(44))) {
            b.a(parcel, 44, imVar.getPostalCode(), true);
        }
        if (hB.contains(Integer.valueOf(45))) {
            b.a(parcel, 45, imVar.getRatingValue(), true);
        }
        if (hB.contains(Integer.valueOf(51))) {
            b.a(parcel, 51, imVar.getThumbnailUrl(), true);
        }
        if (hB.contains(Integer.valueOf(50))) {
            b.a(parcel, 50, imVar.hQ(), i, true);
        }
        if (hB.contains(Integer.valueOf(49))) {
            b.a(parcel, 49, imVar.getText(), true);
        }
        if (hB.contains(Integer.valueOf(48))) {
            b.a(parcel, 48, imVar.getStreetAddress(), true);
        }
        if (hB.contains(Integer.valueOf(55))) {
            b.a(parcel, 55, imVar.getWidth(), true);
        }
        if (hB.contains(Integer.valueOf(54))) {
            b.a(parcel, 54, imVar.getUrl(), true);
        }
        if (hB.contains(Integer.valueOf(53))) {
            b.a(parcel, 53, imVar.getType(), true);
        }
        if (hB.contains(Integer.valueOf(52))) {
            b.a(parcel, 52, imVar.getTickerSymbol(), true);
        }
        if (hB.contains(Integer.valueOf(56))) {
            b.a(parcel, 56, imVar.getWorstRating(), true);
        }
        b.D(parcel, p);
    }

    public im aG(Parcel parcel) {
        int o = a.o(parcel);
        Set hashSet = new HashSet();
        int i = 0;
        im imVar = null;
        List list = null;
        im imVar2 = null;
        String str = null;
        String str2 = null;
        String str3 = null;
        List list2 = null;
        int i2 = 0;
        List list3 = null;
        im imVar3 = null;
        List list4 = null;
        String str4 = null;
        String str5 = null;
        im imVar4 = null;
        String str6 = null;
        String str7 = null;
        String str8 = null;
        List list5 = null;
        String str9 = null;
        String str10 = null;
        String str11 = null;
        String str12 = null;
        String str13 = null;
        String str14 = null;
        String str15 = null;
        String str16 = null;
        String str17 = null;
        im imVar5 = null;
        String str18 = null;
        String str19 = null;
        String str20 = null;
        String str21 = null;
        im imVar6 = null;
        double d = 0.0d;
        im imVar7 = null;
        double d2 = 0.0d;
        String str22 = null;
        im imVar8 = null;
        List list6 = null;
        String str23 = null;
        String str24 = null;
        String str25 = null;
        String str26 = null;
        im imVar9 = null;
        String str27 = null;
        String str28 = null;
        String str29 = null;
        im imVar10 = null;
        String str30 = null;
        String str31 = null;
        String str32 = null;
        String str33 = null;
        String str34 = null;
        String str35 = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            im imVar11;
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(1));
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    imVar11 = (im) a.a(parcel, n, im.CREATOR);
                    hashSet.add(Integer.valueOf(2));
                    imVar = imVar11;
                    break;
                case DetectedActivity.STILL /*3*/:
                    list = a.y(parcel, n);
                    hashSet.add(Integer.valueOf(3));
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    imVar11 = (im) a.a(parcel, n, im.CREATOR);
                    hashSet.add(Integer.valueOf(4));
                    imVar2 = imVar11;
                    break;
                case DetectedActivity.TILTING /*5*/:
                    str = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(5));
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    str2 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(6));
                    break;
                case Error.AVS_DECLINE /*7*/:
                    str3 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(7));
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    list2 = a.c(parcel, n, im.CREATOR);
                    hashSet.add(Integer.valueOf(8));
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    i2 = a.g(parcel, n);
                    hashSet.add(Integer.valueOf(9));
                    break;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    list3 = a.c(parcel, n, im.CREATOR);
                    hashSet.add(Integer.valueOf(10));
                    break;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    imVar11 = (im) a.a(parcel, n, im.CREATOR);
                    hashSet.add(Integer.valueOf(11));
                    imVar3 = imVar11;
                    break;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    list4 = a.c(parcel, n, im.CREATOR);
                    hashSet.add(Integer.valueOf(12));
                    break;
                case CommonStatusCodes.ERROR /*13*/:
                    str4 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(13));
                    break;
                case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                    str5 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(14));
                    break;
                case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                    imVar11 = (im) a.a(parcel, n, im.CREATOR);
                    hashSet.add(Integer.valueOf(15));
                    imVar4 = imVar11;
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                    str6 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(16));
                    break;
                case 17:
                    str7 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(17));
                    break;
                case 18:
                    str8 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(18));
                    break;
                case TimeUtils.HUNDRED_DAY_FIELD_LEN /*19*/:
                    list5 = a.c(parcel, n, im.CREATOR);
                    hashSet.add(Integer.valueOf(19));
                    break;
                case 20:
                    str9 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(20));
                    break;
                case 21:
                    str10 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(21));
                    break;
                case 22:
                    str11 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(22));
                    break;
                case 23:
                    str12 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(23));
                    break;
                case 24:
                    str13 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(24));
                    break;
                case 25:
                    str14 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(25));
                    break;
                case 26:
                    str15 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(26));
                    break;
                case 27:
                    str16 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(27));
                    break;
                case 28:
                    str17 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(28));
                    break;
                case 29:
                    imVar11 = (im) a.a(parcel, n, im.CREATOR);
                    hashSet.add(Integer.valueOf(29));
                    imVar5 = imVar11;
                    break;
                case 30:
                    str18 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(30));
                    break;
                case 31:
                    str19 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(31));
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_STOP /*32*/:
                    str20 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(32));
                    break;
                case 33:
                    str21 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(33));
                    break;
                case 34:
                    imVar11 = (im) a.a(parcel, n, im.CREATOR);
                    hashSet.add(Integer.valueOf(34));
                    imVar6 = imVar11;
                    break;
                case 36:
                    d = a.k(parcel, n);
                    hashSet.add(Integer.valueOf(36));
                    break;
                case 37:
                    imVar11 = (im) a.a(parcel, n, im.CREATOR);
                    hashSet.add(Integer.valueOf(37));
                    imVar7 = imVar11;
                    break;
                case 38:
                    d2 = a.k(parcel, n);
                    hashSet.add(Integer.valueOf(38));
                    break;
                case 39:
                    str22 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(39));
                    break;
                case 40:
                    imVar11 = (im) a.a(parcel, n, im.CREATOR);
                    hashSet.add(Integer.valueOf(40));
                    imVar8 = imVar11;
                    break;
                case 41:
                    list6 = a.c(parcel, n, im.CREATOR);
                    hashSet.add(Integer.valueOf(41));
                    break;
                case 42:
                    str23 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(42));
                    break;
                case 43:
                    str24 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(43));
                    break;
                case 44:
                    str25 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(44));
                    break;
                case 45:
                    str26 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(45));
                    break;
                case 46:
                    imVar11 = (im) a.a(parcel, n, im.CREATOR);
                    hashSet.add(Integer.valueOf(46));
                    imVar9 = imVar11;
                    break;
                case 47:
                    str27 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(47));
                    break;
                case 48:
                    str28 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(48));
                    break;
                case 49:
                    str29 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(49));
                    break;
                case AdSize.PORTRAIT_AD_HEIGHT /*50*/:
                    imVar11 = (im) a.a(parcel, n, im.CREATOR);
                    hashSet.add(Integer.valueOf(50));
                    imVar10 = imVar11;
                    break;
                case 51:
                    str30 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(51));
                    break;
                case 52:
                    str31 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(52));
                    break;
                case 53:
                    str32 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(53));
                    break;
                case 54:
                    str33 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(54));
                    break;
                case 55:
                    str34 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(55));
                    break;
                case 56:
                    str35 = a.m(parcel, n);
                    hashSet.add(Integer.valueOf(56));
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new im(hashSet, i, imVar, list, imVar2, str, str2, str3, list2, i2, list3, imVar3, list4, str4, str5, imVar4, str6, str7, str8, list5, str9, str10, str11, str12, str13, str14, str15, str16, str17, imVar5, str18, str19, str20, str21, imVar6, d, imVar7, d2, str22, imVar8, list6, str23, str24, str25, str26, imVar9, str27, str28, str29, imVar10, str30, str31, str32, str33, str34, str35);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public im[] bD(int i) {
        return new im[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aG(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bD(x0);
    }
}
