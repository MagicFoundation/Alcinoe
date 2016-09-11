package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.drive.Contents;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class z implements Creator<OnContentsResponse> {
    static void a(OnContentsResponse onContentsResponse, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, onContentsResponse.wj);
        b.a(parcel, 2, onContentsResponse.CW, i, false);
        b.D(parcel, p);
    }

    public OnContentsResponse N(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        Contents contents = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    contents = (Contents) a.a(parcel, n, Contents.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new OnContentsResponse(i, contents);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public OnContentsResponse[] as(int i) {
        return new OnContentsResponse[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return N(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return as(x0);
    }
}
