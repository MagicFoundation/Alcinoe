package com.google.android.gms.drive;

import android.os.Parcel;
import android.os.ParcelFileDescriptor;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class a implements Creator<Contents> {
    static void a(Contents contents, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, contents.wj);
        b.a(parcel, 2, contents.AC, i, false);
        b.c(parcel, 3, contents.CQ);
        b.c(parcel, 4, contents.CR);
        b.a(parcel, 5, contents.CS, i, false);
        b.D(parcel, p);
    }

    public Contents[] ad(int i) {
        return new Contents[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return y(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return ad(x0);
    }

    public Contents y(Parcel parcel) {
        DriveId driveId = null;
        int i = 0;
        int o = com.google.android.gms.common.internal.safeparcel.a.o(parcel);
        int i2 = 0;
        ParcelFileDescriptor parcelFileDescriptor = null;
        int i3 = 0;
        while (parcel.dataPosition() < o) {
            int n = com.google.android.gms.common.internal.safeparcel.a.n(parcel);
            switch (com.google.android.gms.common.internal.safeparcel.a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i3 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    parcelFileDescriptor = (ParcelFileDescriptor) com.google.android.gms.common.internal.safeparcel.a.a(parcel, n, ParcelFileDescriptor.CREATOR);
                    break;
                case DetectedActivity.STILL /*3*/:
                    i2 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    i = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    driveId = (DriveId) com.google.android.gms.common.internal.safeparcel.a.a(parcel, n, DriveId.CREATOR);
                    break;
                default:
                    com.google.android.gms.common.internal.safeparcel.a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new Contents(i3, parcelFileDescriptor, i2, i, driveId);
        }
        throw new com.google.android.gms.common.internal.safeparcel.a.a("Overread allowed size end=" + o, parcel);
    }
}
