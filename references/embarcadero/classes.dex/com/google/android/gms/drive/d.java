package com.google.android.gms.drive;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class d implements Creator<DriveId> {
    static void a(DriveId driveId, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, driveId.wj);
        b.a(parcel, 2, driveId.Dc, false);
        b.a(parcel, 3, driveId.Dd);
        b.a(parcel, 4, driveId.De);
        b.D(parcel, p);
    }

    public DriveId[] ae(int i) {
        return new DriveId[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return z(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return ae(x0);
    }

    public DriveId z(Parcel parcel) {
        long j = 0;
        int o = a.o(parcel);
        int i = 0;
        String str = null;
        long j2 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str = a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    j2 = a.h(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    j = a.h(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new DriveId(i, str, j2, j);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }
}
