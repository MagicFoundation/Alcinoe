package com.google.android.gms.common.images;

import android.net.Uri;
import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class b implements Creator<WebImage> {
    static void a(WebImage webImage, Parcel parcel, int i) {
        int p = com.google.android.gms.common.internal.safeparcel.b.p(parcel);
        com.google.android.gms.common.internal.safeparcel.b.c(parcel, 1, webImage.getVersionCode());
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 2, webImage.getUrl(), i, false);
        com.google.android.gms.common.internal.safeparcel.b.c(parcel, 3, webImage.getWidth());
        com.google.android.gms.common.internal.safeparcel.b.c(parcel, 4, webImage.getHeight());
        com.google.android.gms.common.internal.safeparcel.b.D(parcel, p);
    }

    public WebImage[] M(int i) {
        return new WebImage[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return l(x0);
    }

    public WebImage l(Parcel parcel) {
        int i = 0;
        int o = a.o(parcel);
        Uri uri = null;
        int i2 = 0;
        int i3 = 0;
        while (parcel.dataPosition() < o) {
            Uri uri2;
            int g;
            int n = a.n(parcel);
            int i4;
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i4 = i;
                    i = i2;
                    uri2 = uri;
                    g = a.g(parcel, n);
                    n = i4;
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    g = i3;
                    i4 = i2;
                    uri2 = (Uri) a.a(parcel, n, Uri.CREATOR);
                    n = i;
                    i = i4;
                    break;
                case DetectedActivity.STILL /*3*/:
                    uri2 = uri;
                    g = i3;
                    i4 = i;
                    i = a.g(parcel, n);
                    n = i4;
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    n = a.g(parcel, n);
                    i = i2;
                    uri2 = uri;
                    g = i3;
                    break;
                default:
                    a.b(parcel, n);
                    n = i;
                    i = i2;
                    uri2 = uri;
                    g = i3;
                    break;
            }
            i3 = g;
            uri = uri2;
            i2 = i;
            i = n;
        }
        if (parcel.dataPosition() == o) {
            return new WebImage(i3, uri, i2, i);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return M(x0);
    }
}
