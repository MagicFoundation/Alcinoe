package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class ae implements Creator<OnListParentsResponse> {
    static void a(OnListParentsResponse onListParentsResponse, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, onListParentsResponse.wj);
        b.a(parcel, 2, onListParentsResponse.Ee, i, false);
        b.D(parcel, p);
    }

    public OnListParentsResponse S(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        DataHolder dataHolder = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    dataHolder = (DataHolder) a.a(parcel, n, DataHolder.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new OnListParentsResponse(i, dataHolder);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public OnListParentsResponse[] ax(int i) {
        return new OnListParentsResponse[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return S(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return ax(x0);
    }
}
