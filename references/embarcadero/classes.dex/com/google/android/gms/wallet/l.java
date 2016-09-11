package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.identity.intents.model.CountrySpecification;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;

public class l implements Creator<MaskedWalletRequest> {
    static void a(MaskedWalletRequest maskedWalletRequest, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, maskedWalletRequest.getVersionCode());
        b.a(parcel, 2, maskedWalletRequest.Yl, false);
        b.a(parcel, 3, maskedWalletRequest.YY);
        b.a(parcel, 4, maskedWalletRequest.YZ);
        b.a(parcel, 5, maskedWalletRequest.Za);
        b.a(parcel, 6, maskedWalletRequest.Zb, false);
        b.a(parcel, 7, maskedWalletRequest.Yg, false);
        b.a(parcel, 8, maskedWalletRequest.Zc, false);
        b.a(parcel, 9, maskedWalletRequest.Yu, i, false);
        b.a(parcel, 10, maskedWalletRequest.Zd);
        b.a(parcel, 11, maskedWalletRequest.Ze);
        b.a(parcel, 12, maskedWalletRequest.Zf, i, false);
        b.a(parcel, 13, maskedWalletRequest.Zg);
        b.a(parcel, 14, maskedWalletRequest.Zh);
        b.b(parcel, 15, maskedWalletRequest.Zi, false);
        b.D(parcel, p);
    }

    public MaskedWalletRequest bc(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        String str = null;
        boolean z = false;
        boolean z2 = false;
        boolean z3 = false;
        String str2 = null;
        String str3 = null;
        String str4 = null;
        Cart cart = null;
        boolean z4 = false;
        boolean z5 = false;
        CountrySpecification[] countrySpecificationArr = null;
        boolean z6 = true;
        boolean z7 = true;
        ArrayList arrayList = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str = a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    z = a.c(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    z2 = a.c(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    z3 = a.c(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    str2 = a.m(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    str3 = a.m(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    str4 = a.m(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    cart = (Cart) a.a(parcel, n, Cart.CREATOR);
                    break;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    z4 = a.c(parcel, n);
                    break;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    z5 = a.c(parcel, n);
                    break;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    countrySpecificationArr = (CountrySpecification[]) a.b(parcel, n, CountrySpecification.CREATOR);
                    break;
                case CommonStatusCodes.ERROR /*13*/:
                    z6 = a.c(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                    z7 = a.c(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                    arrayList = a.c(parcel, n, CountrySpecification.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new MaskedWalletRequest(i, str, z, z2, z3, str2, str3, str4, cart, z4, z5, countrySpecificationArr, z6, z7, arrayList);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public MaskedWalletRequest[] ci(int i) {
        return new MaskedWalletRequest[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return bc(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return ci(x0);
    }
}
