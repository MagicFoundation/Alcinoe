package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.drive.Contents;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.drive.metadata.internal.MetadataBundle;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class d implements Creator<CloseContentsAndUpdateMetadataRequest> {
    static void a(CloseContentsAndUpdateMetadataRequest closeContentsAndUpdateMetadataRequest, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, closeContentsAndUpdateMetadataRequest.wj);
        b.a(parcel, 2, closeContentsAndUpdateMetadataRequest.Do, i, false);
        b.a(parcel, 3, closeContentsAndUpdateMetadataRequest.Dp, i, false);
        b.a(parcel, 4, closeContentsAndUpdateMetadataRequest.Dq, i, false);
        b.D(parcel, p);
    }

    public CloseContentsAndUpdateMetadataRequest E(Parcel parcel) {
        Contents contents = null;
        int o = a.o(parcel);
        int i = 0;
        MetadataBundle metadataBundle = null;
        DriveId driveId = null;
        while (parcel.dataPosition() < o) {
            MetadataBundle metadataBundle2;
            DriveId driveId2;
            int g;
            Contents contents2;
            int n = a.n(parcel);
            Contents contents3;
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    contents3 = contents;
                    metadataBundle2 = metadataBundle;
                    driveId2 = driveId;
                    g = a.g(parcel, n);
                    contents2 = contents3;
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    g = i;
                    MetadataBundle metadataBundle3 = metadataBundle;
                    driveId2 = (DriveId) a.a(parcel, n, DriveId.CREATOR);
                    contents2 = contents;
                    metadataBundle2 = metadataBundle3;
                    break;
                case DetectedActivity.STILL /*3*/:
                    driveId2 = driveId;
                    g = i;
                    contents3 = contents;
                    metadataBundle2 = (MetadataBundle) a.a(parcel, n, MetadataBundle.CREATOR);
                    contents2 = contents3;
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    contents2 = (Contents) a.a(parcel, n, Contents.CREATOR);
                    metadataBundle2 = metadataBundle;
                    driveId2 = driveId;
                    g = i;
                    break;
                default:
                    a.b(parcel, n);
                    contents2 = contents;
                    metadataBundle2 = metadataBundle;
                    driveId2 = driveId;
                    g = i;
                    break;
            }
            i = g;
            driveId = driveId2;
            metadataBundle = metadataBundle2;
            contents = contents2;
        }
        if (parcel.dataPosition() == o) {
            return new CloseContentsAndUpdateMetadataRequest(i, driveId, metadataBundle, contents);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public CloseContentsAndUpdateMetadataRequest[] aj(int i) {
        return new CloseContentsAndUpdateMetadataRequest[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return E(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return aj(x0);
    }
}
