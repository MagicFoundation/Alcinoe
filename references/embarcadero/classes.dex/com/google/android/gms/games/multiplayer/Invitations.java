package com.google.android.gms.games.multiplayer;

import android.content.Intent;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Releasable;
import com.google.android.gms.common.api.Result;

public interface Invitations {

    public interface LoadInvitationsResult extends Releasable, Result {
        InvitationBuffer getInvitations();
    }

    Intent getInvitationInboxIntent(GoogleApiClient googleApiClient);

    PendingResult<LoadInvitationsResult> loadInvitations(GoogleApiClient googleApiClient);

    PendingResult<LoadInvitationsResult> loadInvitations(GoogleApiClient googleApiClient, int i);

    void registerInvitationListener(GoogleApiClient googleApiClient, OnInvitationReceivedListener onInvitationReceivedListener);

    void unregisterInvitationListener(GoogleApiClient googleApiClient);
}
