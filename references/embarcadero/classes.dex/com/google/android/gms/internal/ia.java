package com.google.android.gms.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.List;

public class ia implements Creator<hz> {
    static void a(hz hzVar, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.a(parcel, 1, hzVar.name, false);
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, hzVar.versionCode);
        b.a(parcel, 2, hzVar.Ov, false);
        b.a(parcel, 3, hzVar.Ow, false);
        b.a(parcel, 4, hzVar.Ox, false);
        b.a(parcel, 5, hzVar.Oy, false);
        b.D(parcel, p);
    }

    public hz aC(Parcel parcel) {
        List list = null;
        int o = a.o(parcel);
        int i = 0;
        String str = null;
        String str2 = null;
        String str3 = null;
        String str4 = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    str4 = a.m(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str3 = a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    str2 = a.m(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    str = a.m(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    list = a.y(parcel, n);
                    break;
                case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE /*1000*/:
                    i = a.g(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new hz(i, str4, str3, str2, str, list);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public hz[] bx(int i) {
        return new hz[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aC(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bx(x0);
    }
}
