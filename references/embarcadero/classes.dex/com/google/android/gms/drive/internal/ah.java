package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class ah implements Creator<OpenFileIntentSenderRequest> {
    static void a(OpenFileIntentSenderRequest openFileIntentSenderRequest, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, openFileIntentSenderRequest.wj);
        b.a(parcel, 2, openFileIntentSenderRequest.CX, false);
        b.a(parcel, 3, openFileIntentSenderRequest.Dk, false);
        b.a(parcel, 4, openFileIntentSenderRequest.CY, i, false);
        b.D(parcel, p);
    }

    public OpenFileIntentSenderRequest V(Parcel parcel) {
        DriveId driveId = null;
        int o = a.o(parcel);
        int i = 0;
        String[] strArr = null;
        String str = null;
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
                    strArr = a.x(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    driveId = (DriveId) a.a(parcel, n, DriveId.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new OpenFileIntentSenderRequest(i, str, strArr, driveId);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public OpenFileIntentSenderRequest[] aA(int i) {
        return new OpenFileIntentSenderRequest[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return V(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return aA(x0);
    }
}
