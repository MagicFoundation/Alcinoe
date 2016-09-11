package com.google.android.gms.drive.query.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.drive.metadata.internal.MetadataBundle;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class a implements Creator<ComparisonFilter> {
    static void a(ComparisonFilter comparisonFilter, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, comparisonFilter.wj);
        b.a(parcel, 1, comparisonFilter.EO, i, false);
        b.a(parcel, 2, comparisonFilter.EP, i, false);
        b.D(parcel, p);
    }

    public ComparisonFilter[] aH(int i) {
        return new ComparisonFilter[i];
    }

    public ComparisonFilter ac(Parcel parcel) {
        MetadataBundle metadataBundle = null;
        int o = com.google.android.gms.common.internal.safeparcel.a.o(parcel);
        int i = 0;
        Operator operator = null;
        while (parcel.dataPosition() < o) {
            int i2;
            MetadataBundle metadataBundle2;
            Operator operator2;
            int n = com.google.android.gms.common.internal.safeparcel.a.n(parcel);
            switch (com.google.android.gms.common.internal.safeparcel.a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i2 = i;
                    Operator operator3 = (Operator) com.google.android.gms.common.internal.safeparcel.a.a(parcel, n, Operator.CREATOR);
                    metadataBundle2 = metadataBundle;
                    operator2 = operator3;
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    metadataBundle2 = (MetadataBundle) com.google.android.gms.common.internal.safeparcel.a.a(parcel, n, MetadataBundle.CREATOR);
                    operator2 = operator;
                    i2 = i;
                    break;
                case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE /*1000*/:
                    MetadataBundle metadataBundle3 = metadataBundle;
                    operator2 = operator;
                    i2 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    metadataBundle2 = metadataBundle3;
                    break;
                default:
                    com.google.android.gms.common.internal.safeparcel.a.b(parcel, n);
                    metadataBundle2 = metadataBundle;
                    operator2 = operator;
                    i2 = i;
                    break;
            }
            i = i2;
            operator = operator2;
            metadataBundle = metadataBundle2;
        }
        if (parcel.dataPosition() == o) {
            return new ComparisonFilter(i, operator, metadataBundle);
        }
        throw new com.google.android.gms.common.internal.safeparcel.a.a("Overread allowed size end=" + o, parcel);
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return ac(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return aH(x0);
    }
}
