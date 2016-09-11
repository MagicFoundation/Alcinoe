package com.google.android.gms.games.multiplayer.realtime;

import android.content.Intent;
import com.google.android.gms.common.api.GoogleApiClient;
import java.util.List;

public interface RealTimeMultiplayer {
    public static final int REAL_TIME_MESSAGE_FAILED = -1;

    public interface ReliableMessageSentCallback {
        void onRealTimeMessageSent(int i, int i2, String str);
    }

    void create(GoogleApiClient googleApiClient, RoomConfig roomConfig);

    void declineInvitation(GoogleApiClient googleApiClient, String str);

    void dismissInvitation(GoogleApiClient googleApiClient, String str);

    Intent getSelectOpponentsIntent(GoogleApiClient googleApiClient, int i, int i2);

    Intent getSelectOpponentsIntent(GoogleApiClient googleApiClient, int i, int i2, boolean z);

    RealTimeSocket getSocketForParticipant(GoogleApiClient googleApiClient, String str, String str2);

    Intent getWaitingRoomIntent(GoogleApiClient googleApiClient, Room room, int i);

    void join(GoogleApiClient googleApiClient, RoomConfig roomConfig);

    void leave(GoogleApiClient googleApiClient, RoomUpdateListener roomUpdateListener, String str);

    int sendReliableMessage(GoogleApiClient googleApiClient, ReliableMessageSentCallback reliableMessageSentCallback, byte[] bArr, String str, String str2);

    int sendUnreliableMessage(GoogleApiClient googleApiClient, byte[] bArr, String str, String str2);

    int sendUnreliableMessage(GoogleApiClient googleApiClient, byte[] bArr, String str, List<String> list);

    int sendUnreliableMessageToOthers(GoogleApiClient googleApiClient, byte[] bArr, String str);
}
