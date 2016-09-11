package com.google.android.gms.internal;

import android.content.Intent;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.common.api.a.c;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.games.Games;
import com.google.android.gms.games.multiplayer.InvitationBuffer;
import com.google.android.gms.games.multiplayer.Invitations;
import com.google.android.gms.games.multiplayer.Invitations.LoadInvitationsResult;
import com.google.android.gms.games.multiplayer.OnInvitationReceivedListener;

public final class gi implements Invitations {

    private static abstract class a extends com.google.android.gms.games.Games.a<LoadInvitationsResult> {

        /* renamed from: com.google.android.gms.internal.gi.a.1 */
        class AnonymousClass1 implements LoadInvitationsResult {
            final /* synthetic */ a HR;
            final /* synthetic */ Status vb;

            AnonymousClass1(a aVar, Status status) {
                this.HR = aVar;
                this.vb = status;
            }

            public InvitationBuffer getInvitations() {
                return new InvitationBuffer(DataHolder.empty(14));
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
            return v(status);
        }

        public LoadInvitationsResult v(Status status) {
            return new AnonymousClass1(this, status);
        }
    }

    /* renamed from: com.google.android.gms.internal.gi.1 */
    class AnonymousClass1 extends a {
        final /* synthetic */ int HP;
        final /* synthetic */ gi HQ;

        AnonymousClass1(gi giVar, int i) {
            this.HQ = giVar;
            this.HP = i;
            super();
        }

        protected void a(fx fxVar) {
            fxVar.c((c) this, this.HP);
        }
    }

    public Intent getInvitationInboxIntent(GoogleApiClient apiClient) {
        return Games.c(apiClient).fu();
    }

    public PendingResult<LoadInvitationsResult> loadInvitations(GoogleApiClient apiClient) {
        return loadInvitations(apiClient, 0);
    }

    public PendingResult<LoadInvitationsResult> loadInvitations(GoogleApiClient apiClient, int sortOrder) {
        return apiClient.a(new AnonymousClass1(this, sortOrder));
    }

    public void registerInvitationListener(GoogleApiClient apiClient, OnInvitationReceivedListener listener) {
        Games.c(apiClient).a(listener);
    }

    public void unregisterInvitationListener(GoogleApiClient apiClient) {
        Games.c(apiClient).fv();
    }
}
