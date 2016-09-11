package com.google.android.gms.games.leaderboard;

import com.google.android.gms.common.data.DataBuffer;
import com.google.android.gms.common.data.DataHolder;

public final class LeaderboardScoreBuffer extends DataBuffer<LeaderboardScore> {
    private final c IO;

    public LeaderboardScoreBuffer(DataHolder dataHolder) {
        super(dataHolder);
        this.IO = new c(dataHolder.getMetadata());
    }

    public c fX() {
        return this.IO;
    }

    public LeaderboardScore get(int position) {
        return new e(this.zU, position);
    }
}
