package com.google.android.gms.location;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class LocationRequestCreator implements Creator<LocationRequest> {
    public static final int CONTENT_DESCRIPTION = 0;

    static void a(LocationRequest locationRequest, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, locationRequest.mPriority);
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, locationRequest.getVersionCode());
        b.a(parcel, 2, locationRequest.Lc);
        b.a(parcel, 3, locationRequest.Ld);
        b.a(parcel, 4, locationRequest.Le);
        b.a(parcel, 5, locationRequest.KV);
        b.c(parcel, 6, locationRequest.Lf);
        b.a(parcel, 7, locationRequest.Lg);
        b.D(parcel, p);
    }

    public LocationRequest createFromParcel(Parcel parcel) {
        boolean z = false;
        int o = a.o(parcel);
        int i = LocationRequest.PRIORITY_BALANCED_POWER_ACCURACY;
        long j = 3600000;
        long j2 = 600000;
        long j3 = Long.MAX_VALUE;
        int i2 = Integer.MAX_VALUE;
        float f = 0.0f;
        int i3 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    j = a.h(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    j2 = a.h(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    z = a.c(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    j3 = a.h(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    i2 = a.g(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    f = a.j(parcel, n);
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
            return new LocationRequest(i3, i, j, j2, z, j3, i2, f);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public LocationRequest[] newArray(int size) {
        return new LocationRequest[size];
    }
}
