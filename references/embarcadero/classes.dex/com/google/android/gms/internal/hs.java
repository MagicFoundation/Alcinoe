package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.gms.location.LocationRequest;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class hs implements Creator<hr> {
    static void a(hr hrVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.a(parcel, 1, hrVar.gu(), i, false);
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, hrVar.wj);
        b.a(parcel, 2, hrVar.gv(), i, false);
        b.D(parcel, p);
    }

    public hr ay(Parcel parcel) {
        hn hnVar = null;
        int o = a.o(parcel);
        int i = 0;
        LocationRequest locationRequest = null;
        while (parcel.dataPosition() < o) {
            int i2;
            hn hnVar2;
            LocationRequest locationRequest2;
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i2 = i;
                    LocationRequest locationRequest3 = (LocationRequest) a.a(parcel, n, LocationRequest.CREATOR);
                    hnVar2 = hnVar;
                    locationRequest2 = locationRequest3;
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    hnVar2 = (hn) a.a(parcel, n, hn.CREATOR);
                    locationRequest2 = locationRequest;
                    i2 = i;
                    break;
                case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE /*1000*/:
                    hn hnVar3 = hnVar;
                    locationRequest2 = locationRequest;
                    i2 = a.g(parcel, n);
                    hnVar2 = hnVar3;
                    break;
                default:
                    a.b(parcel, n);
                    hnVar2 = hnVar;
                    locationRequest2 = locationRequest;
                    i2 = i;
                    break;
            }
            i = i2;
            locationRequest = locationRequest2;
            hnVar = hnVar2;
        }
        if (parcel.dataPosition() == o) {
            return new hr(i, locationRequest, hnVar);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public hr[] bs(int i) {
        return new hr[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return ay(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bs(x0);
    }
}
