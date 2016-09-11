package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class aj implements Creator<RemoveEventListenerRequest> {
    static void a(RemoveEventListenerRequest removeEventListenerRequest, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, removeEventListenerRequest.wj);
        b.a(parcel, 2, removeEventListenerRequest.CS, i, false);
        b.c(parcel, 3, removeEventListenerRequest.Dm);
        b.D(parcel, p);
    }

    public RemoveEventListenerRequest X(Parcel parcel) {
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
            return new RemoveEventListenerRequest(i2, driveId, i);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public RemoveEventListenerRequest[] aC(int i) {
        return new RemoveEventListenerRequest[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return X(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return aC(x0);
    }
}
