package com.google.android.gms.drive.query.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;
import java.util.List;

public class f implements Creator<LogicalFilter> {
    static void a(LogicalFilter logicalFilter, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, logicalFilter.wj);
        b.a(parcel, 1, logicalFilter.EO, i, false);
        b.b(parcel, 2, logicalFilter.EY, false);
        b.D(parcel, p);
    }

    public LogicalFilter[] aL(int i) {
        return new LogicalFilter[i];
    }

    public LogicalFilter ag(Parcel parcel) {
        List list = null;
        int o = a.o(parcel);
        int i = 0;
        Operator operator = null;
        while (parcel.dataPosition() < o) {
            int i2;
            Operator operator2;
            ArrayList c;
            int n = a.n(parcel);
            List list2;
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i2 = i;
                    Operator operator3 = (Operator) a.a(parcel, n, Operator.CREATOR);
                    list2 = list;
                    operator2 = operator3;
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    c = a.c(parcel, n, FilterHolder.CREATOR);
                    operator2 = operator;
                    i2 = i;
                    break;
                case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE /*1000*/:
                    List list3 = list;
                    operator2 = operator;
                    i2 = a.g(parcel, n);
                    list2 = list3;
                    break;
                default:
                    a.b(parcel, n);
                    c = list;
                    operator2 = operator;
                    i2 = i;
                    break;
            }
            i = i2;
            operator = operator2;
            Object obj = c;
        }
        if (parcel.dataPosition() == o) {
            return new LogicalFilter(i, operator, list);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return ag(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return aL(x0);
    }
}
