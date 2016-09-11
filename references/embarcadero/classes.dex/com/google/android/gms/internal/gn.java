package com.google.android.gms.internal;

import android.content.Intent;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.games.Games;
import com.google.android.gms.games.multiplayer.realtime.RealTimeMultiplayer;
import com.google.android.gms.games.multiplayer.realtime.RealTimeMultiplayer.ReliableMessageSentCallback;
import com.google.android.gms.games.multiplayer.realtime.RealTimeSocket;
import com.google.android.gms.games.multiplayer.realtime.Room;
import com.google.android.gms.games.multiplayer.realtime.RoomConfig;
import com.google.android.gms.games.multiplayer.realtime.RoomUpdateListener;
import java.util.List;

public final class gn implements RealTimeMultiplayer {
    public void create(GoogleApiClient apiClient, RoomConfig config) {
        Games.c(apiClient).a(config);
    }

    public void declineInvitation(GoogleApiClient apiClient, String invitationId) {
        Games.c(apiClient).m(invitationId, 0);
    }

    public void dismissInvitation(GoogleApiClient apiClient, String invitationId) {
        Games.c(apiClient).l(invitationId, 0);
    }

    public Intent getSelectOpponentsIntent(GoogleApiClient apiClient, int minPlayers, int maxPlayers) {
        return Games.c(apiClient).b(minPlayers, maxPlayers, true);
    }

    public Intent getSelectOpponentsIntent(GoogleApiClient apiClient, int minPlayers, int maxPlayers, boolean allowAutomatch) {
        return Games.c(apiClient).b(minPlayers, maxPlayers, allowAutomatch);
    }

    public RealTimeSocket getSocketForParticipant(GoogleApiClient apiClient, String roomId, String participantId) {
        return Games.c(apiClient).i(roomId, participantId);
    }

    public Intent getWaitingRoomIntent(GoogleApiClient apiClient, Room room, int minParticipantsToStart) {
        return Games.c(apiClient).a(room, minParticipantsToStart);
    }

    public void join(GoogleApiClient apiClient, RoomConfig config) {
        Games.c(apiClient).b(config);
    }

    public void leave(GoogleApiClient apiClient, RoomUpdateListener listener, String roomId) {
        Games.c(apiClient).a(listener, roomId);
    }

    public int sendReliableMessage(GoogleApiClient apiClient, ReliableMessageSentCallback callback, byte[] messageData, String roomId, String recipientParticipantId) {
        return Games.c(apiClient).a(callback, messageData, roomId, recipientParticipantId);
    }

    public int sendUnreliableMessage(GoogleApiClient apiClient, byte[] messageData, String roomId, String recipientParticipantId) {
        return Games.c(apiClient).a(messageData, roomId, new String[]{recipientParticipantId});
    }

    public int sendUnreliableMessage(GoogleApiClient apiClient, byte[] messageData, String roomId, List<String> recipientParticipantIds) {
        return Games.c(apiClient).a(messageData, roomId, (String[]) recipientParticipantIds.toArray(new String[recipientParticipantIds.size()]));
    }

    public int sendUnreliableMessageToOthers(GoogleApiClient apiClient, byte[] messageData, String roomId) {
        return Games.c(apiClient).d(messageData, roomId);
    }
}
