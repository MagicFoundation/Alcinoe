package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.drive.metadata.internal.MetadataBundle;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class am implements Creator<UpdateMetadataRequest> {
    static void a(UpdateMetadataRequest updateMetadataRequest, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, updateMetadataRequest.wj);
        b.a(parcel, 2, updateMetadataRequest.Do, i, false);
        b.a(parcel, 3, updateMetadataRequest.Dp, i, false);
        b.D(parcel, p);
    }

    public UpdateMetadataRequest Z(Parcel parcel) {
        MetadataBundle metadataBundle = null;
        int o = a.o(parcel);
        int i = 0;
        DriveId driveId = null;
        while (parcel.dataPosition() < o) {
            DriveId driveId2;
            int g;
            MetadataBundle metadataBundle2;
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    MetadataBundle metadataBundle3 = metadataBundle;
                    driveId2 = driveId;
                    g = a.g(parcel, n);
                    metadataBundle2 = metadataBundle3;
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    g = i;
                    DriveId driveId3 = (DriveId) a.a(parcel, n, DriveId.CREATOR);
                    metadataBundle2 = metadataBundle;
                    driveId2 = driveId3;
                    break;
                case DetectedActivity.STILL /*3*/:
                    metadataBundle2 = (MetadataBundle) a.a(parcel, n, MetadataBundle.CREATOR);
                    driveId2 = driveId;
                    g = i;
                    break;
                default:
                    a.b(parcel, n);
                    metadataBundle2 = metadataBundle;
                    driveId2 = driveId;
                    g = i;
                    break;
            }
            i = g;
            driveId = driveId2;
            metadataBundle = metadataBundle2;
        }
        if (parcel.dataPosition() == o) {
            return new UpdateMetadataRequest(i, driveId, metadataBundle);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public UpdateMetadataRequest[] aE(int i) {
        return new UpdateMetadataRequest[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return Z(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return aE(x0);
    }
}
