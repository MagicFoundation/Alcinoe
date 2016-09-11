package com.google.android.gms.internal;

import android.content.Intent;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.common.api.a.c;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.games.Games;
import com.google.android.gms.games.Player;
import com.google.android.gms.games.PlayerBuffer;
import com.google.android.gms.games.Players;
import com.google.android.gms.games.Players.LoadPlayersResult;

public final class gm implements Players {

    private static abstract class a extends com.google.android.gms.games.Games.a<LoadPlayersResult> {

        /* renamed from: com.google.android.gms.internal.gm.a.1 */
        class AnonymousClass1 implements LoadPlayersResult {
            final /* synthetic */ a Ii;
            final /* synthetic */ Status vb;

            AnonymousClass1(a aVar, Status status) {
                this.Ii = aVar;
                this.vb = status;
            }

            public PlayerBuffer getPlayers() {
                return new PlayerBuffer(DataHolder.empty(14));
            }

            public Status getStatus() {
                return this.vb;
            }

            public void release() {
            }
        }

        private a() {
        }

        public LoadPlayersResult A(Status status) {
            return new AnonymousClass1(this, status);
        }

        public /* synthetic */ Result d(Status status) {
            return A(status);
        }
    }

    /* renamed from: com.google.android.gms.internal.gm.1 */
    class AnonymousClass1 extends a {
        final /* synthetic */ String If;
        final /* synthetic */ gm Ig;

        AnonymousClass1(gm gmVar, String str) {
            this.Ig = gmVar;
            this.If = str;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((c) this, this.If);
        }
    }

    /* renamed from: com.google.android.gms.internal.gm.2 */
    class AnonymousClass2 extends a {
        final /* synthetic */ boolean HH;
        final /* synthetic */ gm Ig;
        final /* synthetic */ int Ih;

        AnonymousClass2(gm gmVar, int i, boolean z) {
            this.Ig = gmVar;
            this.Ih = i;
            this.HH = z;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((c) this, this.Ih, false, this.HH);
        }
    }

    /* renamed from: com.google.android.gms.internal.gm.3 */
    class AnonymousClass3 extends a {
        final /* synthetic */ gm Ig;
        final /* synthetic */ int Ih;

        AnonymousClass3(gm gmVar, int i) {
            this.Ig = gmVar;
            this.Ih = i;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((c) this, this.Ih, true, false);
        }
    }

    /* renamed from: com.google.android.gms.internal.gm.4 */
    class AnonymousClass4 extends a {
        final /* synthetic */ boolean HH;
        final /* synthetic */ gm Ig;
        final /* synthetic */ int Ih;

        AnonymousClass4(gm gmVar, int i, boolean z) {
            this.Ig = gmVar;
            this.Ih = i;
            this.HH = z;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((c) this, "playedWith", this.Ih, false, this.HH);
        }
    }

    /* renamed from: com.google.android.gms.internal.gm.5 */
    class AnonymousClass5 extends a {
        final /* synthetic */ gm Ig;
        final /* synthetic */ int Ih;

        AnonymousClass5(gm gmVar, int i) {
            this.Ig = gmVar;
            this.Ih = i;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((c) this, "playedWith", this.Ih, true, false);
        }
    }

    /* renamed from: com.google.android.gms.internal.gm.6 */
    class AnonymousClass6 extends a {
        final /* synthetic */ boolean HH;
        final /* synthetic */ gm Ig;

        AnonymousClass6(gm gmVar, boolean z) {
            this.Ig = gmVar;
            this.HH = z;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.a((c) this, this.HH);
        }
    }

    public Player getCurrentPlayer(GoogleApiClient apiClient) {
        return Games.c(apiClient).fp();
    }

    public String getCurrentPlayerId(GoogleApiClient apiClient) {
        return Games.c(apiClient).fo();
    }

    public Intent getPlayerSearchIntent(GoogleApiClient apiClient) {
        return Games.c(apiClient).fy();
    }

    public PendingResult<LoadPlayersResult> loadConnectedPlayers(GoogleApiClient apiClient, boolean forceReload) {
        return apiClient.a(new AnonymousClass6(this, forceReload));
    }

    public PendingResult<LoadPlayersResult> loadInvitablePlayers(GoogleApiClient apiClient, int pageSize, boolean forceReload) {
        return apiClient.a(new AnonymousClass2(this, pageSize, forceReload));
    }

    public PendingResult<LoadPlayersResult> loadMoreInvitablePlayers(GoogleApiClient apiClient, int pageSize) {
        return apiClient.a(new AnonymousClass3(this, pageSize));
    }

    public PendingResult<LoadPlayersResult> loadMoreRecentlyPlayedWithPlayers(GoogleApiClient apiClient, int pageSize) {
        return apiClient.a(new AnonymousClass5(this, pageSize));
    }

    public PendingResult<LoadPlayersResult> loadPlayer(GoogleApiClient apiClient, String playerId) {
        return apiClient.a(new AnonymousClass1(this, playerId));
    }

    public PendingResult<LoadPlayersResult> loadRecentlyPlayedWithPlayers(GoogleApiClient apiClient, int pageSize, boolean forceReload) {
        return apiClient.a(new AnonymousClass4(this, pageSize, forceReload));
    }
}
