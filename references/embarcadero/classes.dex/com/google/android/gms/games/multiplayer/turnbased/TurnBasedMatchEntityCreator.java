package com.google.android.gms.games.multiplayer.turnbased;

import android.os.Bundle;
import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import android.support.v4.util.TimeUtils;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.internal.safeparcel.a;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.GameEntity;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.games.multiplayer.ParticipantEntity;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.ArrayList;

public class TurnBasedMatchEntityCreator implements Creator<TurnBasedMatchEntity> {
    public static final int CONTENT_DESCRIPTION = 0;

    static void a(TurnBasedMatchEntity turnBasedMatchEntity, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.a(parcel, 1, turnBasedMatchEntity.getGame(), i, false);
        b.a(parcel, 2, turnBasedMatchEntity.getMatchId(), false);
        b.a(parcel, 3, turnBasedMatchEntity.getCreatorId(), false);
        b.a(parcel, 4, turnBasedMatchEntity.getCreationTimestamp());
        b.a(parcel, 5, turnBasedMatchEntity.getLastUpdaterId(), false);
        b.a(parcel, 6, turnBasedMatchEntity.getLastUpdatedTimestamp());
        b.a(parcel, 7, turnBasedMatchEntity.getPendingParticipantId(), false);
        b.c(parcel, 8, turnBasedMatchEntity.getStatus());
        b.c(parcel, 10, turnBasedMatchEntity.getVariant());
        b.c(parcel, 11, turnBasedMatchEntity.getVersion());
        b.a(parcel, 12, turnBasedMatchEntity.getData(), false);
        b.b(parcel, 13, turnBasedMatchEntity.getParticipants(), false);
        b.a(parcel, 14, turnBasedMatchEntity.getRematchId(), false);
        b.a(parcel, 15, turnBasedMatchEntity.getPreviousMatchData(), false);
        b.a(parcel, 17, turnBasedMatchEntity.getAutoMatchCriteria(), false);
        b.c(parcel, 16, turnBasedMatchEntity.getMatchNumber());
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, turnBasedMatchEntity.getVersionCode());
        b.a(parcel, 19, turnBasedMatchEntity.isLocallyModified());
        b.c(parcel, 18, turnBasedMatchEntity.getTurnStatus());
        b.a(parcel, 21, turnBasedMatchEntity.getDescriptionParticipantId(), false);
        b.a(parcel, 20, turnBasedMatchEntity.getDescription(), false);
        b.D(parcel, p);
    }

    public TurnBasedMatchEntity createFromParcel(Parcel parcel) {
        int o = a.o(parcel);
        int i = 0;
        GameEntity gameEntity = null;
        String str = null;
        String str2 = null;
        long j = 0;
        String str3 = null;
        long j2 = 0;
        String str4 = null;
        int i2 = 0;
        int i3 = 0;
        int i4 = 0;
        byte[] bArr = null;
        ArrayList arrayList = null;
        String str5 = null;
        byte[] bArr2 = null;
        int i5 = 0;
        Bundle bundle = null;
        int i6 = 0;
        boolean z = false;
        String str6 = null;
        String str7 = null;
        while (parcel.dataPosition() < o) {
            int n = a.n(parcel);
            switch (a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    gameEntity = (GameEntity) a.a(parcel, n, GameEntity.CREATOR);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str = a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    str2 = a.m(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    j = a.h(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    str3 = a.m(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    j2 = a.h(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    str4 = a.m(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    i2 = a.g(parcel, n);
                    break;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    i3 = a.g(parcel, n);
                    break;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    i4 = a.g(parcel, n);
                    break;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    bArr = a.p(parcel, n);
                    break;
                case CommonStatusCodes.ERROR /*13*/:
                    arrayList = a.c(parcel, n, ParticipantEntity.CREATOR);
                    break;
                case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                    str5 = a.m(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                    bArr2 = a.p(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                    i5 = a.g(parcel, n);
                    break;
                case 17:
                    bundle = a.o(parcel, n);
                    break;
                case 18:
                    i6 = a.g(parcel, n);
                    break;
                case TimeUtils.HUNDRED_DAY_FIELD_LEN /*19*/:
                    z = a.c(parcel, n);
                    break;
                case 20:
                    str6 = a.m(parcel, n);
                    break;
                case 21:
                    str7 = a.m(parcel, n);
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
            return new TurnBasedMatchEntity(i, gameEntity, str, str2, j, str3, j2, str4, i2, i3, i4, bArr, arrayList, str5, bArr2, i5, bundle, i6, z, str6, str7);
        }
        throw new a.a("Overread allowed size end=" + o, parcel);
    }

    public TurnBasedMatchEntity[] newArray(int size) {
        return new TurnBasedMatchEntity[size];
    }
}
