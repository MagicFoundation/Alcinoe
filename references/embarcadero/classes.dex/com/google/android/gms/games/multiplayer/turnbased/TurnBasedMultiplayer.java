package com.google.android.gms.games.multiplayer.turnbased;

import android.content.Intent;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Releasable;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.games.multiplayer.ParticipantResult;
import java.util.List;

public interface TurnBasedMultiplayer {

    public interface CancelMatchResult extends Result {
        String getMatchId();
    }

    public interface InitiateMatchResult extends Result {
        TurnBasedMatch getMatch();
    }

    public interface LeaveMatchResult extends Result {
        TurnBasedMatch getMatch();
    }

    public interface LoadMatchResult extends Result {
        TurnBasedMatch getMatch();
    }

    public interface LoadMatchesResult extends Releasable, Result {
        LoadMatchesResponse getMatches();
    }

    public interface UpdateMatchResult extends Result {
        TurnBasedMatch getMatch();
    }

    PendingResult<InitiateMatchResult> acceptInvitation(GoogleApiClient googleApiClient, String str);

    PendingResult<CancelMatchResult> cancelMatch(GoogleApiClient googleApiClient, String str);

    PendingResult<InitiateMatchResult> createMatch(GoogleApiClient googleApiClient, TurnBasedMatchConfig turnBasedMatchConfig);

    void declineInvitation(GoogleApiClient googleApiClient, String str);

    void dismissInvitation(GoogleApiClient googleApiClient, String str);

    void dismissMatch(GoogleApiClient googleApiClient, String str);

    PendingResult<UpdateMatchResult> finishMatch(GoogleApiClient googleApiClient, String str);

    PendingResult<UpdateMatchResult> finishMatch(GoogleApiClient googleApiClient, String str, byte[] bArr, List<ParticipantResult> list);

    PendingResult<UpdateMatchResult> finishMatch(GoogleApiClient googleApiClient, String str, byte[] bArr, ParticipantResult... participantResultArr);

    Intent getInboxIntent(GoogleApiClient googleApiClient);

    int getMaxMatchDataSize(GoogleApiClient googleApiClient);

    Intent getSelectOpponentsIntent(GoogleApiClient googleApiClient, int i, int i2);

    Intent getSelectOpponentsIntent(GoogleApiClient googleApiClient, int i, int i2, boolean z);

    PendingResult<LeaveMatchResult> leaveMatch(GoogleApiClient googleApiClient, String str);

    PendingResult<LeaveMatchResult> leaveMatchDuringTurn(GoogleApiClient googleApiClient, String str, String str2);

    PendingResult<LoadMatchResult> loadMatch(GoogleApiClient googleApiClient, String str);

    PendingResult<LoadMatchesResult> loadMatchesByStatus(GoogleApiClient googleApiClient, int i, int[] iArr);

    PendingResult<LoadMatchesResult> loadMatchesByStatus(GoogleApiClient googleApiClient, int[] iArr);

    void registerMatchUpdateListener(GoogleApiClient googleApiClient, OnTurnBasedMatchUpdateReceivedListener onTurnBasedMatchUpdateReceivedListener);

    PendingResult<InitiateMatchResult> rematch(GoogleApiClient googleApiClient, String str);

    PendingResult<UpdateMatchResult> takeTurn(GoogleApiClient googleApiClient, String str, byte[] bArr, String str2);

    PendingResult<UpdateMatchResult> takeTurn(GoogleApiClient googleApiClient, String str, byte[] bArr, String str2, List<ParticipantResult> list);

    PendingResult<UpdateMatchResult> takeTurn(GoogleApiClient googleApiClient, String str, byte[] bArr, String str2, ParticipantResult... participantResultArr);

    void unregisterMatchUpdateListener(GoogleApiClient googleApiClient);
}
