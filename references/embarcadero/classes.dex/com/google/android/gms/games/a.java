package com.google.android.gms.games;

import android.net.Uri;
import android.os.Parcel;
import android.os.Parcelable.Creator;
import android.support.v4.media.TransportMediator;
import android.support.v4.util.TimeUtils;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.internal.safeparcel.b;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.location.GeofenceStatusCodes;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class a implements Creator<GameEntity> {
    static void a(GameEntity gameEntity, Parcel parcel, int i) {
        int p = b.p(parcel);
        b.a(parcel, 1, gameEntity.getApplicationId(), false);
        b.a(parcel, 2, gameEntity.getDisplayName(), false);
        b.a(parcel, 3, gameEntity.getPrimaryCategory(), false);
        b.a(parcel, 4, gameEntity.getSecondaryCategory(), false);
        b.a(parcel, 5, gameEntity.getDescription(), false);
        b.a(parcel, 6, gameEntity.getDeveloperName(), false);
        b.a(parcel, 7, gameEntity.getIconImageUri(), i, false);
        b.a(parcel, 8, gameEntity.getHiResImageUri(), i, false);
        b.a(parcel, 9, gameEntity.getFeaturedImageUri(), i, false);
        b.a(parcel, 10, gameEntity.isPlayEnabledGame());
        b.a(parcel, 11, gameEntity.isInstanceInstalled());
        b.a(parcel, 12, gameEntity.getInstancePackageName(), false);
        b.c(parcel, 13, gameEntity.getGameplayAclStatus());
        b.c(parcel, 14, gameEntity.getAchievementTotalCount());
        b.c(parcel, 15, gameEntity.getLeaderboardCount());
        b.a(parcel, 17, gameEntity.isTurnBasedMultiplayerEnabled());
        b.a(parcel, 16, gameEntity.isRealTimeMultiplayerEnabled());
        b.c(parcel, GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE, gameEntity.getVersionCode());
        b.a(parcel, 19, gameEntity.getHiResImageUrl(), false);
        b.a(parcel, 18, gameEntity.getIconImageUrl(), false);
        b.a(parcel, 21, gameEntity.isMuted());
        b.a(parcel, 20, gameEntity.getFeaturedImageUrl(), false);
        b.D(parcel, p);
    }

    public GameEntity[] aP(int i) {
        return new GameEntity[i];
    }

    public GameEntity aj(Parcel parcel) {
        int o = com.google.android.gms.common.internal.safeparcel.a.o(parcel);
        int i = 0;
        String str = null;
        String str2 = null;
        String str3 = null;
        String str4 = null;
        String str5 = null;
        String str6 = null;
        Uri uri = null;
        Uri uri2 = null;
        Uri uri3 = null;
        boolean z = false;
        boolean z2 = false;
        String str7 = null;
        int i2 = 0;
        int i3 = 0;
        int i4 = 0;
        boolean z3 = false;
        boolean z4 = false;
        String str8 = null;
        String str9 = null;
        String str10 = null;
        boolean z5 = false;
        while (parcel.dataPosition() < o) {
            int n = com.google.android.gms.common.internal.safeparcel.a.n(parcel);
            switch (com.google.android.gms.common.internal.safeparcel.a.S(n)) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    str = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str2 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case DetectedActivity.STILL /*3*/:
                    str3 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case DetectedActivity.UNKNOWN /*4*/:
                    str4 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case DetectedActivity.TILTING /*5*/:
                    str5 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    str6 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case Error.AVS_DECLINE /*7*/:
                    uri = (Uri) com.google.android.gms.common.internal.safeparcel.a.a(parcel, n, Uri.CREATOR);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    uri2 = (Uri) com.google.android.gms.common.internal.safeparcel.a.a(parcel, n, Uri.CREATOR);
                    break;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    uri3 = (Uri) com.google.android.gms.common.internal.safeparcel.a.a(parcel, n, Uri.CREATOR);
                    break;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    z = com.google.android.gms.common.internal.safeparcel.a.c(parcel, n);
                    break;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    z2 = com.google.android.gms.common.internal.safeparcel.a.c(parcel, n);
                    break;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    str7 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case CommonStatusCodes.ERROR /*13*/:
                    i2 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                    i3 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                    i4 = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                    z3 = com.google.android.gms.common.internal.safeparcel.a.c(parcel, n);
                    break;
                case 17:
                    z4 = com.google.android.gms.common.internal.safeparcel.a.c(parcel, n);
                    break;
                case 18:
                    str8 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case TimeUtils.HUNDRED_DAY_FIELD_LEN /*19*/:
                    str9 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case 20:
                    str10 = com.google.android.gms.common.internal.safeparcel.a.m(parcel, n);
                    break;
                case 21:
                    z5 = com.google.android.gms.common.internal.safeparcel.a.c(parcel, n);
                    break;
                case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE /*1000*/:
                    i = com.google.android.gms.common.internal.safeparcel.a.g(parcel, n);
                    break;
                default:
                    com.google.android.gms.common.internal.safeparcel.a.b(parcel, n);
                    break;
            }
        }
        if (parcel.dataPosition() == o) {
            return new GameEntity(i, str, str2, str3, str4, str5, str6, uri, uri2, uri3, z, z2, str7, i2, i3, i4, z3, z4, str8, str9, str10, z5);
        }
        throw new com.google.android.gms.common.internal.safeparcel.a.a("Overread allowed size end=" + o, parcel);
    }

    public /* synthetic */ Object createFromParcel(Parcel x0) {
        return aj(x0);
    }

    public /* synthetic */ Object[] newArray(int x0) {
        return aP(x0);
    }
}
