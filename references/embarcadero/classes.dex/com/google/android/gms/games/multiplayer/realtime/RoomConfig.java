package com.google.android.gms.games.multiplayer.realtime;

import android.os.Bundle;
import com.google.android.gms.games.multiplayer.Multiplayer;
import com.google.android.gms.internal.er;
import java.util.ArrayList;
import java.util.Arrays;

public final class RoomConfig {
    private final String GP;
    private final RoomUpdateListener JK;
    private final RoomStatusUpdateListener JL;
    private final RealTimeMessageReceivedListener JM;
    private final String[] JN;
    private final Bundle JO;
    private final boolean JP;
    private final int Jv;

    public static final class Builder {
        final RoomUpdateListener JK;
        RoomStatusUpdateListener JL;
        RealTimeMessageReceivedListener JM;
        Bundle JO;
        boolean JP;
        String JQ;
        ArrayList<String> JR;
        int Jv;

        private Builder(RoomUpdateListener updateListener) {
            this.JQ = null;
            this.Jv = -1;
            this.JR = new ArrayList();
            this.JP = false;
            this.JK = (RoomUpdateListener) er.b((Object) updateListener, (Object) "Must provide a RoomUpdateListener");
        }

        public Builder addPlayersToInvite(ArrayList<String> playerIds) {
            er.f(playerIds);
            this.JR.addAll(playerIds);
            return this;
        }

        public Builder addPlayersToInvite(String... playerIds) {
            er.f(playerIds);
            this.JR.addAll(Arrays.asList(playerIds));
            return this;
        }

        public RoomConfig build() {
            return new RoomConfig();
        }

        public Builder setAutoMatchCriteria(Bundle autoMatchCriteria) {
            this.JO = autoMatchCriteria;
            return this;
        }

        public Builder setInvitationIdToAccept(String invitationId) {
            er.f(invitationId);
            this.JQ = invitationId;
            return this;
        }

        public Builder setMessageReceivedListener(RealTimeMessageReceivedListener listener) {
            this.JM = listener;
            return this;
        }

        public Builder setRoomStatusUpdateListener(RoomStatusUpdateListener listener) {
            this.JL = listener;
            return this;
        }

        public Builder setSocketCommunicationEnabled(boolean enableSockets) {
            this.JP = enableSockets;
            return this;
        }

        public Builder setVariant(int variant) {
            boolean z = variant == -1 || variant > 0;
            er.b(z, (Object) "Variant must be a positive integer or Room.ROOM_VARIANT_ANY");
            this.Jv = variant;
            return this;
        }
    }

    private RoomConfig(Builder builder) {
        this.JK = builder.JK;
        this.JL = builder.JL;
        this.JM = builder.JM;
        this.GP = builder.JQ;
        this.Jv = builder.Jv;
        this.JO = builder.JO;
        this.JP = builder.JP;
        this.JN = (String[]) builder.JR.toArray(new String[builder.JR.size()]);
        if (this.JM == null) {
            er.a(this.JP, "Must either enable sockets OR specify a message listener");
        }
    }

    public static Builder builder(RoomUpdateListener listener) {
        return new Builder(null);
    }

    public static Bundle createAutoMatchCriteria(int minAutoMatchPlayers, int maxAutoMatchPlayers, long exclusiveBitMask) {
        Bundle bundle = new Bundle();
        bundle.putInt(Multiplayer.EXTRA_MIN_AUTOMATCH_PLAYERS, minAutoMatchPlayers);
        bundle.putInt(Multiplayer.EXTRA_MAX_AUTOMATCH_PLAYERS, maxAutoMatchPlayers);
        bundle.putLong(Multiplayer.EXTRA_EXCLUSIVE_BIT_MASK, exclusiveBitMask);
        return bundle;
    }

    public Bundle getAutoMatchCriteria() {
        return this.JO;
    }

    public String getInvitationId() {
        return this.GP;
    }

    public String[] getInvitedPlayerIds() {
        return this.JN;
    }

    public RealTimeMessageReceivedListener getMessageReceivedListener() {
        return this.JM;
    }

    public RoomStatusUpdateListener getRoomStatusUpdateListener() {
        return this.JL;
    }

    public RoomUpdateListener getRoomUpdateListener() {
        return this.JK;
    }

    public int getVariant() {
        return this.Jv;
    }

    public boolean isSocketEnabled() {
        return this.JP;
    }
}
