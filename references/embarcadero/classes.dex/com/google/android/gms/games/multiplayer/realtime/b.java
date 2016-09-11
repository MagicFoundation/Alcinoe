package com.google.android.gms.games.multiplayer.realtime;

import android.os.Bundle;
import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.games.multiplayer.ParticipantEntity;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;

public class b implements Creator<RoomEntity> {
    static void a(RoomEntity roomEntity, Parcel parcel, int i) {
        int p = com.google.android.gms.common.internal.safeparcel.b.p(parcel);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 1, roomEntity.getRoomId(), false);
        com.google.android.gms.common.internal.safeparcel.b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, roomEntity.getVersionCode());
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 2, roomEntity.getCreatorId(), false);
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 3, roomEntity.getCreationTimestamp());
        com.google.android.gms.common.internal.safeparcel.b.c(parcel, 4, roomEntity.getStatus());
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 5, roomEntity.getDescription(), false);
        com.google.android.gms.common.internal.safeparcel.b.c(parcel, 6, roomEntity.getVariant());
        com.google.android.gms.common.internal.safeparcel.b.a(parcel, 7, roomEntity.getAutoMatchCriteria(), false);
        com.google.android.gms.common.internal.safeparcel.b.b(parcel, 8, roomEntity.getParticipants(), false);
        com.google.android.gms.common.internal.safeparcel.b.c(parcel, 9, roomEntity.getAutoMatchWaitEstimateSeconds());
        com.google.android.gms.common.internal.safeparcel.b.D(parcel, p);
    }

    public RoomEntity aq(Parcel parcel) {
        int i = 0;
        ArrayList arrayList = null;
        int o = a.o(parcel);
        long j = 0;
        Bundle bundle = null;
        int i2 = 0;
        String str = null;
        int i3 = 0;
        String str2 = null;
        String str3 = null;
        int i4 = 0;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    str3 = a.m(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str2 = a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    j = a.h(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    i3 = a.g(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    str = a.m(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    i2 = a.g(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    bundle = a.o(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    arrayList = a.c(parcel, n, ParticipantEntity.CREATOR);
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    i = a.g(parcel, n);
                    break;
                case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE /*1000*/:
                    i4 = a.g(parcel, n);
                    break;
                default:
                    a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new RoomEntity(i4, str3, str2, j, i3, str, i2, bundle, arrayList, i);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public RoomEntity[] bd(int i) {
        return new RoomEntity[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aq(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return bd(x0);
    }
}
