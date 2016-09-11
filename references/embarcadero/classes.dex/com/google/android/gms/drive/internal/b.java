package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class b implements Creator<AuthorizeAccessRequest> {
    static void a(AuthorizeAccessRequest authorizeAccessRequest, Parcel parcel, int i) {
        int p = com.google.android.gms.common.internal.safeparcel.b.p(parcel);
        com.google.android.gms.common.internal.safeparcel.b.c(parcel, 1, authorizeAccessRequest.wj);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 2, authorizeAccessRequest.Dn);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 3, authorizeAccessRequest.CS, i, false);
        com.google.android.gms.common.internal.safeparcel.b.D(parcel, p);
    }

    public AuthorizeAccessRequest D(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        long j = 0;
        DriveId driveId = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    j = a.h(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    driveId = (DriveId) a.a(parcel, n, DriveId.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new AuthorizeAccessRequest(i, j, driveId);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public AuthorizeAccessRequest[] ai(int i) {
        return new AuthorizeAccessRequest[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return D(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return ai(x0);
    }
}
