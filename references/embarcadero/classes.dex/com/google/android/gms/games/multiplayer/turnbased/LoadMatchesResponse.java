package com.google.android.gms.games.multiplayer.turnbased;

import android.os.Bundle;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.games.multiplayer.InvitationBuffer;
import com.google.android.gms.internal.gv;

public final class LoadMatchesResponse {
    private final InvitationBuffer JV;
    private final TurnBasedMatchBuffer JW;
    private final TurnBasedMatchBuffer JX;
    private final TurnBasedMatchBuffer JY;

    public LoadMatchesResponse(Bundle matchData) {
        DataHolder a = a(matchData, 0);
        if (a != null) {
            this.JV = new InvitationBuffer(a);
        } else {
            this.JV = null;
        }
        a = a(matchData, 1);
        if (a != null) {
            this.JW = new TurnBasedMatchBuffer(a);
        } else {
            this.JW = null;
        }
        a = a(matchData, 2);
        if (a != null) {
            this.JX = new TurnBasedMatchBuffer(a);
        } else {
            this.JX = null;
        }
        a = a(matchData, 3);
        if (a != null) {
            this.JY = new TurnBasedMatchBuffer(a);
        } else {
            this.JY = null;
        }
    }

    private static DataHolder a(Bundle bundle, int i) {
        String aW = gv.aW(i);
        return !bundle.containsKey(aW) ? null : (DataHolder) bundle.getParcelable(aW);
    }

    public void close() {
        if (this.JV != null) {
            this.JV.close();
        }
        if (this.JW != null) {
            this.JW.close();
        }
        if (this.JX != null) {
            this.JX.close();
        }
        if (this.JY != null) {
            this.JY.close();
        }
    }

    public TurnBasedMatchBuffer getCompletedMatches() {
        return this.JY;
    }

    public InvitationBuffer getInvitations() {
        return this.JV;
    }

    public TurnBasedMatchBuffer getMyTurnMatches() {
        return this.JW;
    }

    public TurnBasedMatchBuffer getTheirTurnMatches() {
        return this.JX;
    }

    public boolean hasData() {
        return (this.JV == null || this.JV.getCount() <= 0) ? (this.JW == null || this.JW.getCount() <= 0) ? (this.JX == null || this.JX.getCount() <= 0) ? this.JY != null && this.JY.getCount() > 0 : true : true : true;
    }
}
