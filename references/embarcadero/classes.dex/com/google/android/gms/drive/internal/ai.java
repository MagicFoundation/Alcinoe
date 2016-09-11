package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.drive.query.Query;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class ai implements Creator<QueryRequest> {
    static void a(QueryRequest queryRequest, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, queryRequest.wj);
        b.a(parcel, 2, queryRequest.Ef, i, false);
        b.D(parcel, p);
    }

    public QueryRequest W(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        Query query = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    query = (Query) a.a(parcel, n, Query.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new QueryRequest(i, query);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public QueryRequest[] aB(int i) {
        return new QueryRequest[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return W(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return aB(x0);
    }
}
