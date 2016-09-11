package com.google.android.gms.common.api;

import android.app.PendingIntent;
import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class StatusCreator implements Creator<Status> {
    public static final int CONTENT_DESCRIPTION = 0;

    static void a(Status status, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, status.getStatusCode());
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, status.getVersionCode());
        b.a(parcel, 2, status.dF(), false);
        b.a(parcel, 3, status.dE(), i, false);
        b.D(parcel, p);
    }

    public Status createFromParcel(Parcel parcel) {
        PendingIntent pendingIntent = null;
        int i = 0;
        int o = a.o(parcel);
        String str = null;
        int i2 = 0;
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
                    pendingIntent = (PendingIntent) a.a(parcel, n, PendingIntent.CREATOR);
                    break;
                case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE /*1000*/:
                    i2 = a.g(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new Status(i2, i, str, pendingIntent);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public Status[] newArray(int size) {
        return new Status[size];
    }
}
