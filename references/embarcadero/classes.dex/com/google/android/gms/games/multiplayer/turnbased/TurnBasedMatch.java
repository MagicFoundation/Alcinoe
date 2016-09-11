package com.google.android.gms.games.multiplayer.turnbased;

import android.database.CharArrayBuffer;
import android.os.Bundle;
import android.os.Parcelable;
import com.google.android.gms.common.data.Freezable;
import com.google.android.gms.games.Game;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.games.multiplayer.Participatable;
import java.util.ArrayList;

public interface TurnBasedMatch extends Parcelable, Freezable<TurnBasedMatch>, Participatable {
    public static final int MATCH_STATUS_ACTIVE = 1;
    public static final int MATCH_STATUS_AUTO_MATCHING = 0;
    public static final int MATCH_STATUS_CANCELED = 4;
    public static final int MATCH_STATUS_COMPLETE = 2;
    public static final int MATCH_STATUS_EXPIRED = 3;
    public static final int[] MATCH_TURN_STATUS_ALL;
    public static final int MATCH_TURN_STATUS_COMPLETE = 3;
    public static final int MATCH_TURN_STATUS_INVITED = 0;
    public static final int MATCH_TURN_STATUS_MY_TURN = 1;
    public static final int MATCH_TURN_STATUS_THEIR_TURN = 2;
    public static final int MATCH_VARIANT_DEFAULT = -1;

    static {
        MATCH_TURN_STATUS_ALL = new int[]{MATCH_TURN_STATUS_INVITED, MATCH_TURN_STATUS_MY_TURN, MATCH_TURN_STATUS_THEIR_TURN, MATCH_TURN_STATUS_COMPLETE};
    }

    boolean canRematch();

    Bundle getAutoMatchCriteria();

    int getAvailableAutoMatchSlots();

    long getCreationTimestamp();

    String getCreatorId();

    byte[] getData();

    String getDescription();

    void getDescription(CharArrayBuffer charArrayBuffer);

    Participant getDescriptionParticipant();

    String getDescriptionParticipantId();

    Game getGame();

    long getLastUpdatedTimestamp();

    String getLastUpdaterId();

    String getMatchId();

    int getMatchNumber();

    Participant getParticipant(String str);

    String getParticipantId(String str);

    ArrayList<String> getParticipantIds();

    int getParticipantStatus(String str);

    String getPendingParticipantId();

    byte[] getPreviousMatchData();

    String getRematchId();

    int getStatus();

    int getTurnStatus();

    int getVariant();

    int getVersion();

    boolean isLocallyModified();
}
