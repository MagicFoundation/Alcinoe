package com.google.android.gms.games.multiplayer;

public interface OnInvitationReceivedListener {
    void onInvitationReceived(Invitation invitation);

    void onInvitationRemoved(String str);
}
