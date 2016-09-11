package com.google.android.gms.cast;

import android.net.Uri;
import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.images.WebImage;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.List;

public class a implements Creator<ApplicationMetadata> {
    static void a(ApplicationMetadata applicationMetadata, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, applicationMetadata.getVersionCode());
        b.a(parcel, 2, applicationMetadata.getApplicationId(), false);
        b.a(parcel, 3, applicationMetadata.getName(), false);
        b.b(parcel, 4, applicationMetadata.getImages(), false);
        b.a(parcel, 5, applicationMetadata.wm, false);
        b.a(parcel, 6, applicationMetadata.getSenderAppIdentifier(), false);
        b.a(parcel, 7, applicationMetadata.cR(), i, false);
        b.D(parcel, p);
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return j(x0);
    }

    public ApplicationMetadata j(Parcel parcel) {
        Uri uri = null;
        int o = com.google.android.gms.common.internal.safeparcel.a.o(parcel);
        int i = 0;
        String str = null;
        List list = null;
        List list2 = null;
        String str2 = null;
        String str3 = null;
        while (parcel.dataPosition() < o) {
            int n = com.google.android.gms.common.internal.safeparcel.a.n(parcel);
            switch (com.google.android.gms.common.internal.safeparcel.a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str3 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    str2 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    list2 = com.google.android.gms.common.internal.safeparcel.a.c(parcel, n, WebImage.CREATOR);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    list = com.google.android.gms.common.internal.safeparcel.a.y(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    str = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    uri = (Uri) com.google.android.gms.common.internal.safeparcel.a.a(parcel, n, Uri.CREATOR);
                    break;
                default:
                    com.google.android.gms.common.internal.safeparcel.a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new ApplicationMetadata(i, str3, str2, list2, list, str, uri);
        }
        throw new com.google.android.gms.common.internal.safeparcel.a.a("Overread allowed size end=" + o, parcel);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return w(x0);
    }

    public ApplicationMetadata[] w(int i) {
        return new ApplicationMetadata[i];
    }
}
