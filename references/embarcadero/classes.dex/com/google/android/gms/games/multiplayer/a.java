package com.google.android.gms.games.multiplayer;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.GameEntity;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;

public class a implements Creator<InvitationEntity> {
    static void a(InvitationEntity invitationEntity, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.a(parcel, 1, invitationEntity.getGame(), i, false);
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, invitationEntity.getVersionCode());
        b.a(parcel, 2, invitationEntity.getInvitationId(), false);
        b.a(parcel, 3, invitationEntity.getCreationTimestamp());
        b.c(parcel, 4, invitationEntity.getInvitationType());
        b.a(parcel, 5, invitationEntity.getInviter(), i, false);
        b.b(parcel, 6, invitationEntity.getParticipants(), false);
        b.c(parcel, 7, invitationEntity.getVariant());
        b.c(parcel, 8, invitationEntity.getAvailableAutoMatchSlots());
        b.D(parcel, p);
    }

    public InvitationEntity an(Parcel parcel) {
        ArrayList arrayList = null;
        int i = 0;
        int o = com.google.android.gms.common.internal.safeparcel.a.o(parcel);
        long j = 0;
        int i2 = 0;
        ParticipantEntity participantEntity = null;
        int i3 = 0;
        String str = null;
        GameEntity gameEntity = null;
        int i4 = 0;
        while (parcel.dataPosition() < o) {
            int n = com.google.android.gms.common.internal.safeparcel.a.n(parcel);
            switch (com.google.android.gms.common.internal.safeparcel.a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    gameEntity = (GameEntity) com.google.android.gms.common.internal.safeparcel.a.a(parcel, n, GameEntity.CREATOR);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    j = com.google.android.gms.common.internal.safeparcel.a.h(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    i3 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    participantEntity = (ParticipantEntity) com.google.android.gms.common.internal.safeparcel.a.a(parcel, n, ParticipantEntity.CREATOR);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    arrayList = com.google.android.gms.common.internal.safeparcel.a.c(parcel, n, ParticipantEntity.CREATOR);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    i2 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    i = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE /*1000*/:
                    i4 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                default:
                    com.google.android.gms.common.internal.safeparcel.a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new InvitationEntity(i4, gameEntity, str, j, i3, participantEntity, arrayList, i2, i);
        }
        throw new com.google.android.gms.common.internal.safeparcel.a.a("Overread allowed size end=" + o, parcel);
    }

    public InvitationEntity[] ba(int i) {
        return new InvitationEntity[i];
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return an(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return ba(x0);
    }
}
