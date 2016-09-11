package com.google.android.gms.drive.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.drive.events.ChangeEvent;
import com.google.android.gms.drive.events.ConflictEvent;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class ac implements Creator<OnEventResponse> {
    static void a(OnEventResponse onEventResponse, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.c(parcel, 1, onEventResponse.wj);
        b.c(parcel, 2, onEventResponse.Dm);
        b.a(parcel, 3, onEventResponse.Eb, i, false);
        b.a(parcel, 4, onEventResponse.Ec, i, false);
        b.D(parcel, p);
    }

    public OnEventResponse Q(Parcel parcel) {
        ConflictEvent conflictEvent = null;
        int i = 0;
        int o = a.o(parcel);
        ChangeEvent changeEvent = null;
        int i2 = 0;
        while (parcel.dataPosition() < o) {
            ChangeEvent changeEvent2;
            int i3;
            ConflictEvent conflictEvent2;
            int n = a.n(parcel);
            ConflictEvent conflictEvent3;
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    conflictEvent3 = conflictEvent;
                    changeEvent2 = changeEvent;
                    i3 = i;
                    i = a.g(parcel, n);
                    conflictEvent2 = conflictEvent3;
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    i = i2;
                    ChangeEvent changeEvent3 = changeEvent;
                    i3 = a.g(parcel, n);
                    conflictEvent2 = conflictEvent;
                    changeEvent2 = changeEvent3;
                    break;
                case DetectedActivity.STILL /*3*/:
                    i3 = i;
                    i = i2;
                    conflictEvent3 = conflictEvent;
                    changeEvent2 = (ChangeEvent) a.a(parcel, n, ChangeEvent.CREATOR);
                    conflictEvent2 = conflictEvent3;
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    conflictEvent2 = (ConflictEvent) a.a(parcel, n, ConflictEvent.CREATOR);
                    changeEvent2 = changeEvent;
                    i3 = i;
                    i = i2;
                    break;
                default:
                    a.b(parcel, n);
                    conflictEvent2 = conflictEvent;
                    changeEvent2 = changeEvent;
                    i3 = i;
                    i = i2;
                    break;
            }
            i2 = i;
            i = i3;
            changeEvent = changeEvent2;
            conflictEvent = conflictEvent2;
        }
        if (parcel.dataPosition() == o) {
            return new OnEventResponse(i2, i, changeEvent, conflictEvent);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public OnEventResponse[] av(int i) {
        return new OnEventResponse[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return Q(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return av(x0);
    }
}
