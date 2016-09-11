package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class m implements Creator<NotifyTransactionStatusRequest> {
    static void a(NotifyTransactionStatusRequest notifyTransactionStatusRequest, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, notifyTransactionStatusRequest.wj);
        b.a(parcel, 2, notifyTransactionStatusRequest.Yk, false);
        b.c(parcel, 3, notifyTransactionStatusRequest.status);
        b.a(parcel, 4, notifyTransactionStatusRequest.Zk, false);
        b.D(parcel, p);
    }

    public NotifyTransactionStatusRequest bd(Parcel parcel) {
        String str = null;
        int i = 0;
        int o = a.o(parcel);
        String str2 = null;
        int i2 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i2 = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str2 = a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    str = a.m(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new NotifyTransactionStatusRequest(i2, str2, i, str);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public NotifyTransactionStatusRequest[] cj(int i) {
        return new NotifyTransactionStatusRequest[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return bd(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return cj(x0);
    }
}
