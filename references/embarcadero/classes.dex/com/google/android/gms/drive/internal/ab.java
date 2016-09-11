package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class ab implements Creator<OnDriveIdResponse> {
    static void a(OnDriveIdResponse onDriveIdResponse, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, onDriveIdResponse.wj);
        b.a(parcel, 2, onDriveIdResponse.Do, i, false);
        b.D(parcel, p);
    }

    public OnDriveIdResponse P(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        DriveId driveId = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    driveId = (DriveId) a.a(parcel, n, DriveId.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new OnDriveIdResponse(i, driveId);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public OnDriveIdResponse[] au(int i) {
        return new OnDriveIdResponse[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return P(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return au(x0);
    }
}
