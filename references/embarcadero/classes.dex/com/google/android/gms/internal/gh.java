package com.google.android.gms.internal;

import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.games.Game;
import com.google.android.gms.games.GameBuffer;
import com.google.android.gms.games.Games;
import com.google.android.gms.games.GamesMetadata;
import com.google.android.gms.games.GamesMetadata.LoadGamesResult;

public final class gh implements GamesMetadata {

    private static abstract class a extends com.google.android.gms.games.Games.a<LoadGamesResult> {

        /* renamed from: com.google.android.gms.internal.gh.a.1 */
        class AnonymousClass1 implements LoadGamesResult {
            final /* synthetic */ a HO;
            final /* synthetic */ Status vb;

            AnonymousClass1(a aVar, Status status) {
                this.HO = aVar;
                this.vb = status;
            }

            public GameBuffer getGames() {
                return new GameBuffer(DataHolder.empty(14));
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
            return u(status);
        }

        public LoadGamesResult u(Status status) {
            return new AnonymousClass1(this, status);
        }
    }

    public Game getCurrentGame(GoogleApiClient apiClient) {
        return Games.c(apiClient).fq();
    }

    public PendingResult<LoadGamesResult> loadGame(GoogleApiClient apiClient) {
        return apiClient.a(new a() {
            final /* synthetic */ gh HN;

            {
                this.HN = r2;
            }

            protected void a(fx fxVar) {
                fxVar.g(this);
            }
        });
    }
}
