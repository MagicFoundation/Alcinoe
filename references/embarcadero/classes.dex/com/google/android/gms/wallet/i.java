package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class i implements Creator<LineItem> {
    static void a(LineItem lineItem, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, lineItem.getVersionCode());
        b.a(parcel, 2, lineItem.description, false);
        b.a(parcel, 3, lineItem.Yy, false);
        b.a(parcel, 4, lineItem.Yz, false);
        b.a(parcel, 5, lineItem.Yf, false);
        b.c(parcel, 6, lineItem.YA);
        b.a(parcel, 7, lineItem.Yg, false);
        b.D(parcel, p);
    }

    public LineItem aZ(Parcel parcel) {
        int i = 0;
        String str = null;
        int o = a.o(parcel);
        String str2 = null;
        String str3 = null;
        String str4 = null;
        String str5 = null;
        int i2 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i2 = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str5 = a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    str4 = a.m(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    str3 = a.m(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    str2 = a.m(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    i = a.g(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    str = a.m(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new LineItem(i2, str5, str4, str3, str2, i, str);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public LineItem[] cf(int i) {
        return new LineItem[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aZ(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return cf(x0);
    }
}
