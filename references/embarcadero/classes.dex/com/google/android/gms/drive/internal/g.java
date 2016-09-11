package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.drive.metadata.internal.MetadataBundle;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class g implements Creator<CreateFileIntentSenderRequest> {
    static void a(CreateFileIntentSenderRequest createFileIntentSenderRequest, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, createFileIntentSenderRequest.wj);
        b.a(parcel, 2, createFileIntentSenderRequest.Ds, i, false);
        b.c(parcel, 3, createFileIntentSenderRequest.CQ);
        b.a(parcel, 4, createFileIntentSenderRequest.CX, false);
        b.a(parcel, 5, createFileIntentSenderRequest.CY, i, false);
        b.D(parcel, p);
    }

    public CreateFileIntentSenderRequest H(Parcel parcel) {
        int i = 0;
        DriveId driveId = null;
        int o = a.o(parcel);
        String str = null;
        MetadataBundle metadataBundle = null;
        int i2 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i2 = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    metadataBundle = (MetadataBundle) a.a(parcel, n, MetadataBundle.CREATOR);
                    break;
                case DetectedActivity.STILL /*3*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    str = a.m(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    driveId = (DriveId) a.a(parcel, n, DriveId.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new CreateFileIntentSenderRequest(i2, metadataBundle, i, str, driveId);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public CreateFileIntentSenderRequest[] am(int i) {
        return new CreateFileIntentSenderRequest[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return H(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return am(x0);
    }
}
