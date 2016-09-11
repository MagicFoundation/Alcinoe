package com.google.android.gms.games.leaderboard;

import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.common.data.b;

public final class g extends b implements LeaderboardVariant {
    g(DataHolder dataHolder, int i) {
        super(dataHolder, i);
    }

    public boolean equals(Object obj) {
        return f.a(this, obj);
    }

    public /* synthetic */ Object freeze() {
        return gd();
    }

    public String ga() {
        return getString("top_page_token_next");
    }

    public String gb() {
        return getString("window_page_token_prev");
    }

    public String gc() {
        return getString("window_page_token_next");
    }

    public LeaderboardVariant gd() {
        return new f(this);
    }

    public int getCollection() {
        return getInteger("collection");
    }

    public String getDisplayPlayerRank() {
        return getString("player_display_rank");
    }

    public String getDisplayPlayerScore() {
        return getString("player_display_score");
    }

    public long getNumScores() {
        return ab("total_scores") ? -1 : getLong("total_scores");
    }

    public long getPlayerRank() {
        return ab("player_rank") ? -1 : getLong("player_rank");
    }

    public String getPlayerScoreTag() {
        return getString("player_score_tag");
    }

    public long getRawPlayerScore() {
        return ab("player_raw_score") ? -1 : getLong("player_raw_score");
    }

    public int getTimeSpan() {
        return getInteger("timespan");
    }

    public boolean hasPlayerInfo() {
        return !ab("player_raw_score");
    }

    public int hashCode() {
        return f.a(this);
    }

    public String toString() {
        return f.b(this);
    }
}
