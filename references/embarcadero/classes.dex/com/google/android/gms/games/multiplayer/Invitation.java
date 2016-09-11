package com.google.android.gms.games.multiplayer;

import android.os.Parcelable;
import com.google.android.gms.common.data.Freezable;
import com.google.android.gms.games.Game;

public interface Invitation extends Parcelable, Freezable<Invitation>, Participatable {
    public static final int INVITATION_TYPE_REAL_TIME = 0;
    public static final int INVITATION_TYPE_TURN_BASED = 1;

    int getAvailableAutoMatchSlots();

    long getCreationTimestamp();

    Game getGame();

    String getInvitationId();

    int getInvitationType();

    Participant getInviter();

    int getVariant();
}
