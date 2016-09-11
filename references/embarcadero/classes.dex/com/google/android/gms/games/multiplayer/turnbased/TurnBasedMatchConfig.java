package com.google.android.gms.games.multiplayer.turnbased;

import android.os.Bundle;
import com.google.android.gms.games.multiplayer.Multiplayer;
import com.google.android.gms.internal.er;
import java.util.ArrayList;

public final class TurnBasedMatchConfig {
    private final String[] JN;
    private final Bundle JO;
    private final int JZ;
    private final int Jv;

    public static final class Builder {
        Bundle JO;
        ArrayList<String> JR;
        int JZ;
        int Jv;

        private Builder() {
            this.Jv = -1;
            this.JR = new ArrayList();
            this.JO = null;
            this.JZ = 2;
        }

        public Builder addInvitedPlayer(String playerId) {
            er.f(playerId);
            this.JR.add(playerId);
            return this;
        }

        public Builder addInvitedPlayers(ArrayList<String> playerIds) {
            er.f(playerIds);
            this.JR.addAll(playerIds);
            return this;
        }

        public TurnBasedMatchConfig build() {
            return new TurnBasedMatchConfig();
        }

        public Builder setAutoMatchCriteria(Bundle autoMatchCriteria) {
            this.JO = autoMatchCriteria;
            return this;
        }

        public Builder setMinPlayers(int minPlayers) {
            this.JZ = minPlayers;
            return this;
        }

        public Builder setVariant(int variant) {
            boolean z = variant == -1 || variant > 0;
            er.b(z, (Object) "Variant must be a positive integer or TurnBasedMatch.MATCH_VARIANT_ANY");
            this.Jv = variant;
            return this;
        }
    }

    private TurnBasedMatchConfig(Builder builder) {
        this.Jv = builder.Jv;
        this.JZ = builder.JZ;
        this.JO = builder.JO;
        this.JN = (String[]) builder.JR.toArray(new String[builder.JR.size()]);
    }

    public static Builder builder() {
        return new Builder();
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

    public String[] getInvitedPlayerIds() {
        return this.JN;
    }

    public int getMinPlayers() {
        return this.JZ;
    }

    public int getVariant() {
        return this.Jv;
    }
}
