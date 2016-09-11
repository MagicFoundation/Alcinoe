package com.google.android.gms.games.multiplayer;

import com.google.android.gms.games.Player;
import com.google.android.gms.internal.er;
import java.util.ArrayList;

public final class ParticipantUtils {
    private ParticipantUtils() {
    }

    public static boolean aE(String str) {
        er.b((Object) str, (Object) "Participant ID must not be null");
        return str.startsWith("p_");
    }

    public static String getParticipantId(ArrayList<Participant> participants, String playerId) {
        int size = participants.size();
        for (int i = 0; i < size; i++) {
            Participant participant = (Participant) participants.get(i);
            Player player = participant.getPlayer();
            if (player != null && player.getPlayerId().equals(playerId)) {
                return participant.getParticipantId();
            }
        }
        return null;
    }
}
