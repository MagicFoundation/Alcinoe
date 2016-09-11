package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class aa implements Creator<OnDownloadProgressResponse> {
    static void a(OnDownloadProgressResponse onDownloadProgressResponse, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, onDownloadProgressResponse.wj);
        b.a(parcel, 2, onDownloadProgressResponse.DZ);
        b.a(parcel, 3, onDownloadProgressResponse.Ea);
        b.D(parcel, p);
    }

    public OnDownloadProgressResponse O(Parcel parcel) {
        long j = 0;
        int o = a.o(parcel);
        int i = 0;
        long j2 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    j2 = a.h(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    j = a.h(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new OnDownloadProgressResponse(i, j2, j);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public OnDownloadProgressResponse[] at(int i) {
        return new OnDownloadProgressResponse[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return O(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return at(x0);
    }
}
