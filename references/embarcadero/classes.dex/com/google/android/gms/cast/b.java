package com.google.android.gms.cast;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.images.WebImage;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.List;

public class b implements Creator<CastDevice> {
    static void a(CastDevice castDevice, Parcel parcel, int i) {
        int p = com.google.android.gms.common.internal.safeparcel.b.p(parcel);
        com.google.android.gms.common.internal.safeparcel.b.c(parcel, 1, castDevice.getVersionCode());
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 2, castDevice.getDeviceId(), false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 3, castDevice.wD, false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 4, castDevice.getFriendlyName(), false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 5, castDevice.getModelName(), false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 6, castDevice.getDeviceVersion(), false);
        com.google.android.gms.common.internal.safeparcel.b.c(parcel, 7, castDevice.getServicePort());
        com.google.android.gms.common.internal.safeparcel.b.b(parcel, 8, castDevice.getIcons(), false);
        com.google.android.gms.common.internal.safeparcel.b.D(parcel, p);
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return k(x0);
    }

    public CastDevice k(Parcel parcel) {
        int i = 0;
        List list = null;
        int o = a.o(parcel);
        String str = null;
        String str2 = null;
        String str3 = null;
        String str4 = null;
        String str5 = null;
        int i2 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i2 = a.g(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str5 = a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    str4 = a.m(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    str3 = a.m(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    str2 = a.m(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    str = a.m(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    i = a.g(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    list = a.c(parcel, n, WebImage.CREATOR);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new CastDevice(i2, str5, str4, str3, str2, str, i, list);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return y(x0);
    }

    public CastDevice[] y(int i) {
        return new CastDevice[i];
    }
}
