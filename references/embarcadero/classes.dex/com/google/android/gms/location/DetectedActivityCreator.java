package com.google.android.gms.location;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class DetectedActivityCreator implements Creator<DetectedActivity> {
    public static final int CONTENT_DESCRIPTION = 0;

    static void a(DetectedActivity detectedActivity, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, detectedActivity.KS);
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, detectedActivity.getVersionCode());
        b.c(parcel, 2, detectedActivity.KT);
        b.D(parcel, p);
    }

    public DetectedActivity createFromParcel(Parcel parcel) {
        int i = 0;
        int o = a.o(parcel);
        int i2 = 0;
        int i3 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i2 = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    i = a.g(parcel, n);
                    break;
                case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE /*1000*/:
                    i3 = a.g(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new DetectedActivity(i3, i2, i);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public DetectedActivity[] newArray(int size) {
        return new DetectedActivity[size];
    }
}
