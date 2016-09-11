package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class ag implements Creator<OpenContentsRequest> {
    static void a(OpenContentsRequest openContentsRequest, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, openContentsRequest.wj);
        b.a(parcel, 2, openContentsRequest.Do, i, false);
        b.c(parcel, 3, openContentsRequest.CR);
        b.D(parcel, p);
    }

    public OpenContentsRequest U(Parcel parcel) {
        int i = 0;
        int o = a.o(parcel);
        DriveId driveId = null;
        int i2 = 0;
        while (parcel.dataPosition() < o) {
            DriveId driveId2;
            int g;
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    int i3 = i;
                    driveId2 = driveId;
                    g = a.g(parcel, n);
                    n = i3;
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    g = i2;
                    DriveId driveId3 = (DriveId) a.a(parcel, n, DriveId.CREATOR);
                    n = i;
                    driveId2 = driveId3;
                    break;
                case DetectedActivity.STILL /*3*/:
                    n = a.g(parcel, n);
                    driveId2 = driveId;
                    g = i2;
                    break;
                default:
                    a.b(parcel, n);
                    n = i;
                    driveId2 = driveId;
                    g = i2;
                    break;
            }
            i2 = g;
            driveId = driveId2;
            i = n;
        }
        if (parcel.dataPosition() == o) {
            return new OpenContentsRequest(i2, driveId, i);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public OpenContentsRequest[] az(int i) {
        return new OpenContentsRequest[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return U(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return az(x0);
    }
}
