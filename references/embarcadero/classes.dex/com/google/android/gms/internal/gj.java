package com.google.android.gms.internal;

import android.content.Intent;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.games.Games;
import com.google.android.gms.games.leaderboard.Leaderboard;
import com.google.android.gms.games.leaderboard.LeaderboardBuffer;
import com.google.android.gms.games.leaderboard.LeaderboardScore;
import com.google.android.gms.games.leaderboard.LeaderboardScoreBuffer;
import com.google.android.gms.games.leaderboard.Leaderboards;
import com.google.android.gms.games.leaderboard.Leaderboards.LeaderboardMetadataResult;
import com.google.android.gms.games.leaderboard.Leaderboards.LoadPlayerScoreResult;
import com.google.android.gms.games.leaderboard.Leaderboards.LoadScoresResult;
import com.google.android.gms.games.leaderboard.Leaderboards.SubmitScoreResult;
import com.google.android.gms.games.leaderboard.ScoreSubmissionData;

public final class gj implements Leaderboards {

    private static abstract class a extends com.google.android.gms.games.Games.a<LeaderboardMetadataResult> {

        /* renamed from: com.google.android.gms.internal.gj.a.1 */
        class AnonymousClass1 implements LeaderboardMetadataResult {
            final /* synthetic */ a Ib;
            final /* synthetic */ Status vb;

            AnonymousClass1(a aVar, Status status) {
                this.Ib = aVar;
                this.vb = status;
            }

            public LeaderboardBuffer getLeaderboards() {
                return new LeaderboardBuffer(DataHolder.empty(14));
            }

            public Status getStatus() {
                return this.vb;
            }

            public void release() {
            }
        }

        private a() {
        }

        public /* synthetic */ Result d(Status status) {
            return w(status);
        }

        public LeaderboardMetadataResult w(Status status) {
            return new AnonymousClass1(this, status);
        }
    }

    private static abstract class b extends com.google.android.gms.games.Games.a<LoadPlayerScoreResult> {

        /* renamed from: com.google.android.gms.internal.gj.b.1 */
        class AnonymousClass1 implements LoadPlayerScoreResult {
            final /* synthetic */ b Ic;
            final /* synthetic */ Status vb;

            AnonymousClass1(b bVar, Status status) {
                this.Ic = bVar;
                this.vb = status;
            }

            public LeaderboardScore getScore() {
                return null;
            }

            public Status getStatus() {
                return this.vb;
            }
        }

        private b() {
        }

        public /* synthetic */ Result d(Status status) {
            return x(status);
        }

        public LoadPlayerScoreResult x(Status status) {
            return new AnonymousClass1(this, status);
        }
    }

    private static abstract class c extends com.google.android.gms.games.Games.a<LoadScoresResult> {

        /* renamed from: com.google.android.gms.internal.gj.c.1 */
        class AnonymousClass1 implements LoadScoresResult {
            final /* synthetic */ c Id;
            final /* synthetic */ Status vb;

            AnonymousClass1(c cVar, Status status) {
                this.Id = cVar;
                this.vb = status;
            }

            public Leaderboard getLeaderboard() {
                return null;
            }

            public LeaderboardScoreBuffer getScores() {
                return new LeaderboardScoreBuffer(DataHolder.empty(14));
            }

            public Status getStatus() {
                return this.vb;
            }

            public void release() {
            }
        }

        private c() {
        }

        public /* synthetic */ Result d(Status status) {
            return y(status);
        }

        public LoadScoresResult y(Status status) {
            return new AnonymousClass1(this, status);
        }
    }

    protected static abstract class d extends com.google.android.gms.games.Games.a<SubmitScoreResult> {

        /* renamed from: com.google.android.gms.internal.gj.d.1 */
        class AnonymousClass1 implements SubmitScoreResult {
            final /* synthetic */ d Ie;
            final /* synthetic */ Status vb;

            AnonymousClass1(d dVar, Status status) {
                this.Ie = dVar;
                this.vb = status;
            }

            public ScoreSubmissionData getScoreData() {
                return new ScoreSubmissionData(DataHolder.empty(14));
            }

            public Status getStatus() {
                return this.vb;
            }

            public void release() {
            }
        }

        protected d() {
        }

        public /* synthetic */ Result d(Status status) {
            return z(status);
        }

        public SubmitScoreResult z(Status status) {
            return new AnonymousClass1(this, status);
        }
    }

    /* renamed from: com.google.android.gms.internal.gj.1 */
    class AnonymousClass1 extends a {
        final /* synthetic */ boolean HH;
        final /* synthetic */ gj HS;

        AnonymousClass1(gj gjVar, boolean z) {
            this.HS = gjVar;
            this.HH = z;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.b((com.google.android.gms.common.api.a.c) this, this.HH);
        }
    }

    /* renamed from: com.google.android.gms.internal.gj.2 */
    class AnonymousClass2 extends a {
        final /* synthetic */ boolean HH;
        final /* synthetic */ gj HS;
        final /* synthetic */ String HT;

        AnonymousClass2(gj gjVar, String str, boolean z) {
            this.HS = gjVar;
            this.HT = str;
            this.HH = z;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((com.google.android.gms.common.api.a.c) this, this.HT, this.HH);
        }
    }

    /* renamed from: com.google.android.gms.internal.gj.3 */
    class AnonymousClass3 extends b {
        final /* synthetic */ gj HS;
        final /* synthetic */ String HT;
        final /* synthetic */ int HU;
        final /* synthetic */ int HV;

        AnonymousClass3(gj gjVar, String str, int i, int i2) {
            this.HS = gjVar;
            this.HT = str;
            this.HU = i;
            this.HV = i2;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((com.google.android.gms.common.api.a.c) this, null, this.HT, this.HU, this.HV);
        }
    }

    /* renamed from: com.google.android.gms.internal.gj.4 */
    class AnonymousClass4 extends c {
        final /* synthetic */ boolean HH;
        final /* synthetic */ gj HS;
        final /* synthetic */ String HT;
        final /* synthetic */ int HU;
        final /* synthetic */ int HV;
        final /* synthetic */ int HW;

        AnonymousClass4(gj gjVar, String str, int i, int i2, int i3, boolean z) {
            this.HS = gjVar;
            this.HT = str;
            this.HU = i;
            this.HV = i2;
            this.HW = i3;
            this.HH = z;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a(this, this.HT, this.HU, this.HV, this.HW, this.HH);
        }
    }

    /* renamed from: com.google.android.gms.internal.gj.5 */
    class AnonymousClass5 extends c {
        final /* synthetic */ boolean HH;
        final /* synthetic */ gj HS;
        final /* synthetic */ String HT;
        final /* synthetic */ int HU;
        final /* synthetic */ int HV;
        final /* synthetic */ int HW;

        AnonymousClass5(gj gjVar, String str, int i, int i2, int i3, boolean z) {
            this.HS = gjVar;
            this.HT = str;
            this.HU = i;
            this.HV = i2;
            this.HW = i3;
            this.HH = z;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.b(this, this.HT, this.HU, this.HV, this.HW, this.HH);
        }
    }

    /* renamed from: com.google.android.gms.internal.gj.6 */
    class AnonymousClass6 extends c {
        final /* synthetic */ gj HS;
        final /* synthetic */ int HW;
        final /* synthetic */ LeaderboardScoreBuffer HX;
        final /* synthetic */ int HY;

        AnonymousClass6(gj gjVar, LeaderboardScoreBuffer leaderboardScoreBuffer, int i, int i2) {
            this.HS = gjVar;
            this.HX = leaderboardScoreBuffer;
            this.HW = i;
            this.HY = i2;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((com.google.android.gms.common.api.a.c) this, this.HX, this.HW, this.HY);
        }
    }

    /* renamed from: com.google.android.gms.internal.gj.7 */
    class AnonymousClass7 extends d {
        final /* synthetic */ gj HS;
        final /* synthetic */ String HT;
        final /* synthetic */ long HZ;
        final /* synthetic */ String Ia;

        AnonymousClass7(gj gjVar, String str, long j, String str2) {
            this.HS = gjVar;
            this.HT = str;
            this.HZ = j;
            this.Ia = str2;
        }

        protected void a(fx fxVar) {
            fxVar.a((com.google.android.gms.common.api.a.c) this, this.HT, this.HZ, this.Ia);
        }
    }

    public Intent getAllLeaderboardsIntent(GoogleApiClient apiClient) {
        return Games.c(apiClient).fr();
    }

    public Intent getLeaderboardIntent(GoogleApiClient apiClient, String leaderboardId) {
        return Games.c(apiClient).au(leaderboardId);
    }

    public PendingResult<LoadPlayerScoreResult> loadCurrentPlayerLeaderboardScore(GoogleApiClient apiClient, String leaderboardId, int span, int leaderboardCollection) {
        return apiClient.a(new AnonymousClass3(this, leaderboardId, span, leaderboardCollection));
    }

    public PendingResult<LeaderboardMetadataResult> loadLeaderboardMetadata(GoogleApiClient apiClient, String leaderboardId, boolean forceReload) {
        return apiClient.a(new AnonymousClass2(this, leaderboardId, forceReload));
    }

    public PendingResult<LeaderboardMetadataResult> loadLeaderboardMetadata(GoogleApiClient apiClient, boolean forceReload) {
        return apiClient.a(new AnonymousClass1(this, forceReload));
    }

    public PendingResult<LoadScoresResult> loadMoreScores(GoogleApiClient apiClient, LeaderboardScoreBuffer buffer, int maxResults, int pageDirection) {
        return apiClient.a(new AnonymousClass6(this, buffer, maxResults, pageDirection));
    }

    public PendingResult<LoadScoresResult> loadPlayerCenteredScores(GoogleApiClient apiClient, String leaderboardId, int span, int leaderboardCollection, int maxResults) {
        return loadPlayerCenteredScores(apiClient, leaderboardId, span, leaderboardCollection, maxResults, false);
    }

    public PendingResult<LoadScoresResult> loadPlayerCenteredScores(GoogleApiClient apiClient, String leaderboardId, int span, int leaderboardCollection, int maxResults, boolean forceReload) {
        return apiClient.a(new AnonymousClass5(this, leaderboardId, span, leaderboardCollection, maxResults, forceReload));
    }

    public PendingResult<LoadScoresResult> loadTopScores(GoogleApiClient apiClient, String leaderboardId, int span, int leaderboardCollection, int maxResults) {
        return loadTopScores(apiClient, leaderboardId, span, leaderboardCollection, maxResults, false);
    }

    public PendingResult<LoadScoresResult> loadTopScores(GoogleApiClient apiClient, String leaderboardId, int span, int leaderboardCollection, int maxResults, boolean forceReload) {
        return apiClient.a(new AnonymousClass4(this, leaderboardId, span, leaderboardCollection, maxResults, forceReload));
    }

    public void submitScore(GoogleApiClient apiClient, String leaderboardId, long score) {
        submitScore(apiClient, leaderboardId, score, null);
    }

    public void submitScore(GoogleApiClient apiClient, String leaderboardId, long score, String scoreTag) {
        Games.c(apiClient).a(null, leaderboardId, score, scoreTag);
    }

    public PendingResult<SubmitScoreResult> submitScoreImmediate(GoogleApiClient apiClient, String leaderboardId, long score) {
        return submitScoreImmediate(apiClient, leaderboardId, score, null);
    }

    public PendingResult<SubmitScoreResult> submitScoreImmediate(GoogleApiClient apiClient, String leaderboardId, long score, String scoreTag) {
        return apiClient.b(new AnonymousClass7(this, leaderboardId, score, scoreTag));
    }
}
