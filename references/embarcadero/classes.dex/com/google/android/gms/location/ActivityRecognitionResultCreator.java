package com.google.android.gms.location;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.List;

public class ActivityRecognitionResultCreator implements Creator<ActivityRecognitionResult> {
    public static final int CONTENT_DESCRIPTION = 0;

    static void a(ActivityRecognitionResult activityRecognitionResult, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.b(parcel, 1, activityRecognitionResult.KP, false);
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, activityRecognitionResult.getVersionCode());
        b.a(parcel, 2, activityRecognitionResult.KQ);
        b.a(parcel, 3, activityRecognitionResult.KR);
        b.D(parcel, p);
    }

    public ActivityRecognitionResult createFromParcel(Parcel parcel) {
        long j = 0;
        int o = a.o(parcel);
        int i = 0;
        List list = null;
        long j2 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    list = a.c(parcel, n, DetectedActivity.CREATOR);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    j2 = a.h(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    j = a.h(parcel, n);
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
            return new ActivityRecognitionResult(i, list, j2, j);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public ActivityRecognitionResult[] newArray(int size) {
        return new ActivityRecognitionResult[size];
    }
}
