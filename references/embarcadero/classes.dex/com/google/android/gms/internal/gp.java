package com.google.android.gms.internal;

import android.content.Intent;
import android.os.Bundle;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.games.Games;
import com.google.android.gms.games.multiplayer.ParticipantResult;
import com.google.android.gms.games.multiplayer.turnbased.LoadMatchesResponse;
import com.google.android.gms.games.multiplayer.turnbased.OnTurnBasedMatchUpdateReceivedListener;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMatch;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMatchConfig;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMultiplayer;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMultiplayer.CancelMatchResult;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMultiplayer.InitiateMatchResult;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMultiplayer.LeaveMatchResult;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMultiplayer.LoadMatchResult;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMultiplayer.LoadMatchesResult;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMultiplayer.UpdateMatchResult;
import java.util.List;

public final class gp implements TurnBasedMultiplayer {

    private static abstract class a extends com.google.android.gms.games.Games.a<CancelMatchResult> {
        private final String uS;

        /* renamed from: com.google.android.gms.internal.gp.a.1 */
        class AnonymousClass1 implements CancelMatchResult {
            final /* synthetic */ a Iy;
            final /* synthetic */ Status vb;

            AnonymousClass1(a aVar, Status status) {
                this.Iy = aVar;
                this.vb = status;
            }

            public String getMatchId() {
                return this.Iy.uS;
            }

            public Status getStatus() {
                return this.vb;
            }
        }

        public a(String str) {
            this.uS = str;
        }

        public CancelMatchResult D(Status status) {
            return new AnonymousClass1(this, status);
        }

        public /* synthetic */ Result d(Status status) {
            return D(status);
        }
    }

    private static abstract class b extends com.google.android.gms.games.Games.a<InitiateMatchResult> {

        /* renamed from: com.google.android.gms.internal.gp.b.1 */
        class AnonymousClass1 implements InitiateMatchResult {
            final /* synthetic */ b Iz;
            final /* synthetic */ Status vb;

            AnonymousClass1(b bVar, Status status) {
                this.Iz = bVar;
                this.vb = status;
            }

            public TurnBasedMatch getMatch() {
                return null;
            }

            public Status getStatus() {
                return this.vb;
            }
        }

        private b() {
        }

        public InitiateMatchResult E(Status status) {
            return new AnonymousClass1(this, status);
        }

        public /* synthetic */ Result d(Status status) {
            return E(status);
        }
    }

    private static abstract class c extends com.google.android.gms.games.Games.a<LeaveMatchResult> {

        /* renamed from: com.google.android.gms.internal.gp.c.1 */
        class AnonymousClass1 implements LeaveMatchResult {
            final /* synthetic */ c IA;
            final /* synthetic */ Status vb;

            AnonymousClass1(c cVar, Status status) {
                this.IA = cVar;
                this.vb = status;
            }

            public TurnBasedMatch getMatch() {
                return null;
            }

            public Status getStatus() {
                return this.vb;
            }
        }

        private c() {
        }

        public LeaveMatchResult F(Status status) {
            return new AnonymousClass1(this, status);
        }

        public /* synthetic */ Result d(Status status) {
            return F(status);
        }
    }

    private static abstract class d extends com.google.android.gms.games.Games.a<LoadMatchResult> {

        /* renamed from: com.google.android.gms.internal.gp.d.1 */
        class AnonymousClass1 implements LoadMatchResult {
            final /* synthetic */ d IB;
            final /* synthetic */ Status vb;

            AnonymousClass1(d dVar, Status status) {
                this.IB = dVar;
                this.vb = status;
            }

            public TurnBasedMatch getMatch() {
                return null;
            }

            public Status getStatus() {
                return this.vb;
            }
        }

        private d() {
        }

        public LoadMatchResult G(Status status) {
            return new AnonymousClass1(this, status);
        }

        public /* synthetic */ Result d(Status status) {
            return G(status);
        }
    }

    private static abstract class e extends com.google.android.gms.games.Games.a<LoadMatchesResult> {

        /* renamed from: com.google.android.gms.internal.gp.e.1 */
        class AnonymousClass1 implements LoadMatchesResult {
            final /* synthetic */ e IC;
            final /* synthetic */ Status vb;

            AnonymousClass1(e eVar, Status status) {
                this.IC = eVar;
                this.vb = status;
            }

            public LoadMatchesResponse getMatches() {
                return new LoadMatchesResponse(new Bundle());
            }

            public Status getStatus() {
                return this.vb;
            }

            public void release() {
            }
        }

        private e() {
        }

        public LoadMatchesResult H(Status status) {
            return new AnonymousClass1(this, status);
        }

        public /* synthetic */ Result d(Status status) {
            return H(status);
        }
    }

    private static abstract class f extends com.google.android.gms.games.Games.a<UpdateMatchResult> {

        /* renamed from: com.google.android.gms.internal.gp.f.1 */
        class AnonymousClass1 implements UpdateMatchResult {
            final /* synthetic */ f IE;
            final /* synthetic */ Status vb;

            AnonymousClass1(f fVar, Status status) {
                this.IE = fVar;
                this.vb = status;
            }

            public TurnBasedMatch getMatch() {
                return null;
            }

            public Status getStatus() {
                return this.vb;
            }
        }

        private f() {
        }

        public UpdateMatchResult I(Status status) {
            return new AnonymousClass1(this, status);
        }

        public /* synthetic */ Result d(Status status) {
            return I(status);
        }
    }

    /* renamed from: com.google.android.gms.internal.gp.10 */
    class AnonymousClass10 extends e {
        final /* synthetic */ gp Iq;
        final /* synthetic */ int Iw;
        final /* synthetic */ int[] Ix;

        AnonymousClass10(gp gpVar, int i, int[] iArr) {
            this.Iq = gpVar;
            this.Iw = i;
            this.Ix = iArr;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((com.google.android.gms.common.api.a.c) this, this.Iw, this.Ix);
        }
    }

    /* renamed from: com.google.android.gms.internal.gp.1 */
    class AnonymousClass1 extends b {
        final /* synthetic */ TurnBasedMatchConfig Ip;
        final /* synthetic */ gp Iq;

        AnonymousClass1(gp gpVar, TurnBasedMatchConfig turnBasedMatchConfig) {
            this.Iq = gpVar;
            this.Ip = turnBasedMatchConfig;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((com.google.android.gms.common.api.a.c) this, this.Ip);
        }
    }

    /* renamed from: com.google.android.gms.internal.gp.2 */
    class AnonymousClass2 extends d {
        final /* synthetic */ gp Iq;
        final /* synthetic */ String Ir;

        AnonymousClass2(gp gpVar, String str) {
            this.Iq = gpVar;
            this.Ir = str;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.h(this, this.Ir);
        }
    }

    /* renamed from: com.google.android.gms.internal.gp.3 */
    class AnonymousClass3 extends b {
        final /* synthetic */ gp Iq;
        final /* synthetic */ String Ir;

        AnonymousClass3(gp gpVar, String str) {
            this.Iq = gpVar;
            this.Ir = str;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.d((com.google.android.gms.common.api.a.c) this, this.Ir);
        }
    }

    /* renamed from: com.google.android.gms.internal.gp.4 */
    class AnonymousClass4 extends b {
        final /* synthetic */ gp Iq;
        final /* synthetic */ String Is;

        AnonymousClass4(gp gpVar, String str) {
            this.Iq = gpVar;
            this.Is = str;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.e(this, this.Is);
        }
    }

    /* renamed from: com.google.android.gms.internal.gp.5 */
    class AnonymousClass5 extends f {
        final /* synthetic */ gp Iq;
        final /* synthetic */ String Ir;
        final /* synthetic */ byte[] It;
        final /* synthetic */ String Iu;
        final /* synthetic */ ParticipantResult[] Iv;

        AnonymousClass5(gp gpVar, String str, byte[] bArr, String str2, ParticipantResult[] participantResultArr) {
            this.Iq = gpVar;
            this.Ir = str;
            this.It = bArr;
            this.Iu = str2;
            this.Iv = participantResultArr;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((com.google.android.gms.common.api.a.c) this, this.Ir, this.It, this.Iu, this.Iv);
        }
    }

    /* renamed from: com.google.android.gms.internal.gp.6 */
    class AnonymousClass6 extends f {
        final /* synthetic */ gp Iq;
        final /* synthetic */ String Ir;
        final /* synthetic */ byte[] It;
        final /* synthetic */ ParticipantResult[] Iv;

        AnonymousClass6(gp gpVar, String str, byte[] bArr, ParticipantResult[] participantResultArr) {
            this.Iq = gpVar;
            this.Ir = str;
            this.It = bArr;
            this.Iv = participantResultArr;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((com.google.android.gms.common.api.a.c) this, this.Ir, this.It, this.Iv);
        }
    }

    /* renamed from: com.google.android.gms.internal.gp.7 */
    class AnonymousClass7 extends c {
        final /* synthetic */ gp Iq;
        final /* synthetic */ String Ir;

        AnonymousClass7(gp gpVar, String str) {
            this.Iq = gpVar;
            this.Ir = str;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.f(this, this.Ir);
        }
    }

    /* renamed from: com.google.android.gms.internal.gp.8 */
    class AnonymousClass8 extends c {
        final /* synthetic */ gp Iq;
        final /* synthetic */ String Ir;
        final /* synthetic */ String Iu;

        AnonymousClass8(gp gpVar, String str, String str2) {
            this.Iq = gpVar;
            this.Ir = str;
            this.Iu = str2;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((com.google.android.gms.common.api.a.c) this, this.Ir, this.Iu);
        }
    }

    /* renamed from: com.google.android.gms.internal.gp.9 */
    class AnonymousClass9 extends a {
        final /* synthetic */ gp Iq;
        final /* synthetic */ String Ir;

        AnonymousClass9(gp gpVar, String str, String str2) {
            this.Iq = gpVar;
            this.Ir = str2;
            super(str);
        }

        protected void a(fx fxVar) {
            fxVar.g(this, this.Ir);
        }
    }

    public PendingResult<InitiateMatchResult> acceptInvitation(GoogleApiClient apiClient, String invitationId) {
        return apiClient.b(new AnonymousClass4(this, invitationId));
    }

    public PendingResult<CancelMatchResult> cancelMatch(GoogleApiClient apiClient, String matchId) {
        return apiClient.b(new AnonymousClass9(this, matchId, matchId));
    }

    public PendingResult<InitiateMatchResult> createMatch(GoogleApiClient apiClient, TurnBasedMatchConfig config) {
        return apiClient.b(new AnonymousClass1(this, config));
    }

    public void declineInvitation(GoogleApiClient apiClient, String invitationId) {
        Games.c(apiClient).m(invitationId, 1);
    }

    public void dismissInvitation(GoogleApiClient apiClient, String invitationId) {
        Games.c(apiClient).l(invitationId, 1);
    }

    public void dismissMatch(GoogleApiClient apiClient, String matchId) {
        Games.c(apiClient).av(matchId);
    }

    public PendingResult<UpdateMatchResult> finishMatch(GoogleApiClient apiClient, String matchId) {
        return finishMatch(apiClient, matchId, null, (ParticipantResult[]) null);
    }

    public PendingResult<UpdateMatchResult> finishMatch(GoogleApiClient apiClient, String matchId, byte[] matchData, List<ParticipantResult> results) {
        return finishMatch(apiClient, matchId, matchData, results == null ? null : (ParticipantResult[]) results.toArray(new ParticipantResult[results.size()]));
    }

    public PendingResult<UpdateMatchResult> finishMatch(GoogleApiClient apiClient, String matchId, byte[] matchData, ParticipantResult... results) {
        return apiClient.b(new AnonymousClass6(this, matchId, matchData, results));
    }

    public Intent getInboxIntent(GoogleApiClient apiClient) {
        return Games.c(apiClient).ft();
    }

    public int getMaxMatchDataSize(GoogleApiClient apiClient) {
        return Games.c(apiClient).fC();
    }

    public Intent getSelectOpponentsIntent(GoogleApiClient apiClient, int minPlayers, int maxPlayers) {
        return Games.c(apiClient).a(minPlayers, maxPlayers, true);
    }

    public Intent getSelectOpponentsIntent(GoogleApiClient apiClient, int minPlayers, int maxPlayers, boolean allowAutomatch) {
        return Games.c(apiClient).a(minPlayers, maxPlayers, allowAutomatch);
    }

    public PendingResult<LeaveMatchResult> leaveMatch(GoogleApiClient apiClient, String matchId) {
        return apiClient.b(new AnonymousClass7(this, matchId));
    }

    public PendingResult<LeaveMatchResult> leaveMatchDuringTurn(GoogleApiClient apiClient, String matchId, String pendingParticipantId) {
        return apiClient.b(new AnonymousClass8(this, matchId, pendingParticipantId));
    }

    public PendingResult<LoadMatchResult> loadMatch(GoogleApiClient apiClient, String matchId) {
        return apiClient.a(new AnonymousClass2(this, matchId));
    }

    public PendingResult<LoadMatchesResult> loadMatchesByStatus(GoogleApiClient apiClient, int invitationSortOrder, int[] matchTurnStatuses) {
        return apiClient.a(new AnonymousClass10(this, invitationSortOrder, matchTurnStatuses));
    }

    public PendingResult<LoadMatchesResult> loadMatchesByStatus(GoogleApiClient apiClient, int[] matchTurnStatuses) {
        return loadMatchesByStatus(apiClient, 0, matchTurnStatuses);
    }

    public void registerMatchUpdateListener(GoogleApiClient apiClient, OnTurnBasedMatchUpdateReceivedListener listener) {
        Games.c(apiClient).a(listener);
    }

    public PendingResult<InitiateMatchResult> rematch(GoogleApiClient apiClient, String matchId) {
        return apiClient.b(new AnonymousClass3(this, matchId));
    }

    public PendingResult<UpdateMatchResult> takeTurn(GoogleApiClient apiClient, String matchId, byte[] matchData, String pendingParticipantId) {
        return takeTurn(apiClient, matchId, matchData, pendingParticipantId, (ParticipantResult[]) null);
    }

    public PendingResult<UpdateMatchResult> takeTurn(GoogleApiClient apiClient, String matchId, byte[] matchData, String pendingParticipantId, List<ParticipantResult> results) {
        return takeTurn(apiClient, matchId, matchData, pendingParticipantId, results == null ? null : (ParticipantResult[]) results.toArray(new ParticipantResult[results.size()]));
    }

    public PendingResult<UpdateMatchResult> takeTurn(GoogleApiClient apiClient, String matchId, byte[] matchData, String pendingParticipantId, ParticipantResult... results) {
        return apiClient.b(new AnonymousClass5(this, matchId, matchData, pendingParticipantId, results));
    }

    public void unregisterMatchUpdateListener(GoogleApiClient apiClient) {
        Games.c(apiClient).fw();
    }
}
