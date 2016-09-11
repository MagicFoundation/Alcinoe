package com.google.android.gms.drive.events;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class b implements Creator<ConflictEvent> {
    static void a(ConflictEvent conflictEvent, Parcel parcel, int i) {
        int p = com.google.android.gms.common.internal.safeparcel.b.p(parcel);
        com.google.android.gms.common.internal.safeparcel.b.c(parcel, 1, conflictEvent.wj);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 2, conflictEvent.CS, i, false);
        com.google.android.gms.common.internal.safeparcel.b.D(parcel, p);
    }

    public ConflictEvent B(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        DriveId driveId = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    driveId = (DriveId) a.a(parcel, n, DriveId.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new ConflictEvent(i, driveId);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public ConflictEvent[] ag(int i) {
        return new ConflictEvent[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return B(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return ag(x0);
    }
}
