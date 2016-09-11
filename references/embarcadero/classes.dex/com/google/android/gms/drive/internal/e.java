package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.drive.Contents;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class e implements Creator<CloseContentsRequest> {
    static void a(CloseContentsRequest closeContentsRequest, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, closeContentsRequest.wj);
        b.a(parcel, 2, closeContentsRequest.Dq, i, false);
        b.a(parcel, 3, closeContentsRequest.Dr, false);
        b.D(parcel, p);
    }

    public CloseContentsRequest F(Parcel parcel) {
        Boolean bool = null;
        int o = a.o(parcel);
        int i = 0;
        Contents contents = null;
        while (parcel.dataPosition() < o) {
            Contents contents2;
            int g;
            Boolean bool2;
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    Boolean bool3 = bool;
                    contents2 = contents;
                    g = a.g(parcel, n);
                    bool2 = bool3;
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    g = i;
                    Contents contents3 = (Contents) a.a(parcel, n, Contents.CREATOR);
                    bool2 = bool;
                    contents2 = contents3;
                    break;
                case DetectedActivity.STILL /*3*/:
                    bool2 = a.d(parcel, n);
                    contents2 = contents;
                    g = i;
                    break;
                default:
                    a.b(parcel, n);
                    bool2 = bool;
                    contents2 = contents;
                    g = i;
                    break;
            }
            i = g;
            contents = contents2;
            bool = bool2;
        }
        if (parcel.dataPosition() == o) {
            return new CloseContentsRequest(i, contents, bool);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public CloseContentsRequest[] ak(int i) {
        return new CloseContentsRequest[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return F(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return ak(x0);
    }
}
