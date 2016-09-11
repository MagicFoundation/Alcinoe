package com.google.android.gms.internal;

import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.net.LocalSocket;
import android.net.LocalSocketAddress;
import android.os.Binder;
import android.os.Bundle;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Looper;
import android.os.ParcelFileDescriptor;
import android.os.RemoteException;
import android.view.View;
import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.common.Scopes;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.common.api.Releasable;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.games.Game;
import com.google.android.gms.games.GameBuffer;
import com.google.android.gms.games.GameEntity;
import com.google.android.gms.games.GamesMetadata.LoadGamesResult;
import com.google.android.gms.games.Player;
import com.google.android.gms.games.PlayerBuffer;
import com.google.android.gms.games.PlayerEntity;
import com.google.android.gms.games.Players.LoadPlayersResult;
import com.google.android.gms.games.achievement.AchievementBuffer;
import com.google.android.gms.games.achievement.Achievements.LoadAchievementsResult;
import com.google.android.gms.games.achievement.Achievements.UpdateAchievementResult;
import com.google.android.gms.games.leaderboard.Leaderboard;
import com.google.android.gms.games.leaderboard.LeaderboardBuffer;
import com.google.android.gms.games.leaderboard.LeaderboardScore;
import com.google.android.gms.games.leaderboard.LeaderboardScoreBuffer;
import com.google.android.gms.games.leaderboard.Leaderboards.LeaderboardMetadataResult;
import com.google.android.gms.games.leaderboard.Leaderboards.LoadPlayerScoreResult;
import com.google.android.gms.games.leaderboard.Leaderboards.LoadScoresResult;
import com.google.android.gms.games.leaderboard.Leaderboards.SubmitScoreResult;
import com.google.android.gms.games.leaderboard.ScoreSubmissionData;
import com.google.android.gms.games.multiplayer.Invitation;
import com.google.android.gms.games.multiplayer.InvitationBuffer;
import com.google.android.gms.games.multiplayer.Invitations.LoadInvitationsResult;
import com.google.android.gms.games.multiplayer.OnInvitationReceivedListener;
import com.google.android.gms.games.multiplayer.ParticipantResult;
import com.google.android.gms.games.multiplayer.ParticipantUtils;
import com.google.android.gms.games.multiplayer.realtime.RealTimeMessage;
import com.google.android.gms.games.multiplayer.realtime.RealTimeMessageReceivedListener;
import com.google.android.gms.games.multiplayer.realtime.RealTimeMultiplayer.ReliableMessageSentCallback;
import com.google.android.gms.games.multiplayer.realtime.RealTimeSocket;
import com.google.android.gms.games.multiplayer.realtime.Room;
import com.google.android.gms.games.multiplayer.realtime.RoomConfig;
import com.google.android.gms.games.multiplayer.realtime.RoomEntity;
import com.google.android.gms.games.multiplayer.realtime.RoomStatusUpdateListener;
import com.google.android.gms.games.multiplayer.realtime.RoomUpdateListener;
import com.google.android.gms.games.multiplayer.turnbased.LoadMatchesResponse;
import com.google.android.gms.games.multiplayer.turnbased.OnTurnBasedMatchUpdateReceivedListener;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMatch;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMatchBuffer;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMatchConfig;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMultiplayer.CancelMatchResult;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMultiplayer.InitiateMatchResult;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMultiplayer.LeaveMatchResult;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMultiplayer.LoadMatchResult;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMultiplayer.LoadMatchesResult;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMultiplayer.UpdateMatchResult;
import com.google.android.gms.games.request.GameRequest;
import com.google.android.gms.games.request.GameRequestBuffer;
import com.google.android.gms.games.request.OnRequestReceivedListener;
import com.google.android.gms.games.request.Requests.LoadRequestsResult;
import com.google.android.gms.games.request.Requests.UpdateRequestsResult;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public final class fx extends eh<gb> implements ConnectionCallbacks, OnConnectionFailedListener {
    private boolean GA;
    private boolean GB;
    private int GC;
    private final Binder GD;
    private final long GE;
    private final boolean GF;
    private final int GG;
    private final boolean GH;
    private final String Gv;
    private final Map<String, RealTimeSocket> Gw;
    private PlayerEntity Gx;
    private GameEntity Gy;
    private final gd Gz;
    private final String vi;

    final class aa extends b<RoomStatusUpdateListener> {
        final /* synthetic */ fx GJ;
        private final String GZ;

        aa(fx fxVar, RoomStatusUpdateListener roomStatusUpdateListener, String str) {
            this.GJ = fxVar;
            super(fxVar, roomStatusUpdateListener);
            this.GZ = str;
        }

        public void a(RoomStatusUpdateListener roomStatusUpdateListener) {
            if (roomStatusUpdateListener != null) {
                roomStatusUpdateListener.onP2PConnected(this.GZ);
            }
        }

        protected void cP() {
        }
    }

    final class ab extends b<RoomStatusUpdateListener> {
        final /* synthetic */ fx GJ;
        private final String GZ;

        ab(fx fxVar, RoomStatusUpdateListener roomStatusUpdateListener, String str) {
            this.GJ = fxVar;
            super(fxVar, roomStatusUpdateListener);
            this.GZ = str;
        }

        public void a(RoomStatusUpdateListener roomStatusUpdateListener) {
            if (roomStatusUpdateListener != null) {
                roomStatusUpdateListener.onP2PDisconnected(this.GZ);
            }
        }

        protected void cP() {
        }
    }

    final class am extends b<ReliableMessageSentCallback> {
        final /* synthetic */ fx GJ;
        private final String Hc;
        private final int Hd;
        private final int yJ;

        am(fx fxVar, ReliableMessageSentCallback reliableMessageSentCallback, int i, int i2, String str) {
            this.GJ = fxVar;
            super(fxVar, reliableMessageSentCallback);
            this.yJ = i;
            this.Hd = i2;
            this.Hc = str;
        }

        public void a(ReliableMessageSentCallback reliableMessageSentCallback) {
            if (reliableMessageSentCallback != null) {
                reliableMessageSentCallback.onRealTimeMessageSent(this.yJ, this.Hd, this.Hc);
            }
        }

        protected void cP() {
        }
    }

    final class ap extends b<OnRequestReceivedListener> {
        final /* synthetic */ fx GJ;
        private final GameRequest Hg;

        ap(fx fxVar, OnRequestReceivedListener onRequestReceivedListener, GameRequest gameRequest) {
            this.GJ = fxVar;
            super(fxVar, onRequestReceivedListener);
            this.Hg = gameRequest;
        }

        protected /* synthetic */ void a(Object obj) {
            b((OnRequestReceivedListener) obj);
        }

        protected void b(OnRequestReceivedListener onRequestReceivedListener) {
            onRequestReceivedListener.onRequestReceived(this.Hg);
        }

        protected void cP() {
        }
    }

    final class aq extends b<OnRequestReceivedListener> {
        final /* synthetic */ fx GJ;
        private final String Hh;

        aq(fx fxVar, OnRequestReceivedListener onRequestReceivedListener, String str) {
            this.GJ = fxVar;
            super(fxVar, onRequestReceivedListener);
            this.Hh = str;
        }

        protected /* synthetic */ void a(Object obj) {
            b((OnRequestReceivedListener) obj);
        }

        protected void b(OnRequestReceivedListener onRequestReceivedListener) {
            onRequestReceivedListener.onRequestRemoved(this.Hh);
        }

        protected void cP() {
        }
    }

    final class bc extends b<com.google.android.gms.common.api.a.c<Status>> {
        final /* synthetic */ fx GJ;
        private final Status vl;

        public bc(fx fxVar, com.google.android.gms.common.api.a.c<Status> cVar, Status status) {
            this.GJ = fxVar;
            super(fxVar, cVar);
            this.vl = status;
        }

        public /* synthetic */ void a(Object obj) {
            c((com.google.android.gms.common.api.a.c) obj);
        }

        public void c(com.google.android.gms.common.api.a.c<Status> cVar) {
            cVar.b(this.vl);
        }

        protected void cP() {
        }
    }

    final class m extends b<OnInvitationReceivedListener> {
        final /* synthetic */ fx GJ;
        private final Invitation GO;

        m(fx fxVar, OnInvitationReceivedListener onInvitationReceivedListener, Invitation invitation) {
            this.GJ = fxVar;
            super(fxVar, onInvitationReceivedListener);
            this.GO = invitation;
        }

        protected /* synthetic */ void a(Object obj) {
            b((OnInvitationReceivedListener) obj);
        }

        protected void b(OnInvitationReceivedListener onInvitationReceivedListener) {
            onInvitationReceivedListener.onInvitationReceived(this.GO);
        }

        protected void cP() {
        }
    }

    final class n extends b<OnInvitationReceivedListener> {
        final /* synthetic */ fx GJ;
        private final String GP;

        n(fx fxVar, OnInvitationReceivedListener onInvitationReceivedListener, String str) {
            this.GJ = fxVar;
            super(fxVar, onInvitationReceivedListener);
            this.GP = str;
        }

        protected /* synthetic */ void a(Object obj) {
            b((OnInvitationReceivedListener) obj);
        }

        protected void b(OnInvitationReceivedListener onInvitationReceivedListener) {
            onInvitationReceivedListener.onInvitationRemoved(this.GP);
        }

        protected void cP() {
        }
    }

    final class v extends b<RoomUpdateListener> {
        final /* synthetic */ fx GJ;
        private final String GU;
        private final int yJ;

        v(fx fxVar, RoomUpdateListener roomUpdateListener, int i, String str) {
            this.GJ = fxVar;
            super(fxVar, roomUpdateListener);
            this.yJ = i;
            this.GU = str;
        }

        public void a(RoomUpdateListener roomUpdateListener) {
            roomUpdateListener.onLeftRoom(this.yJ, this.GU);
        }

        protected void cP() {
        }
    }

    final class w extends b<OnTurnBasedMatchUpdateReceivedListener> {
        final /* synthetic */ fx GJ;
        private final String GV;

        w(fx fxVar, OnTurnBasedMatchUpdateReceivedListener onTurnBasedMatchUpdateReceivedListener, String str) {
            this.GJ = fxVar;
            super(fxVar, onTurnBasedMatchUpdateReceivedListener);
            this.GV = str;
        }

        protected /* synthetic */ void a(Object obj) {
            b((OnTurnBasedMatchUpdateReceivedListener) obj);
        }

        protected void b(OnTurnBasedMatchUpdateReceivedListener onTurnBasedMatchUpdateReceivedListener) {
            onTurnBasedMatchUpdateReceivedListener.onTurnBasedMatchRemoved(this.GV);
        }

        protected void cP() {
        }
    }

    final class y extends b<OnTurnBasedMatchUpdateReceivedListener> {
        final /* synthetic */ fx GJ;
        private final TurnBasedMatch GX;

        y(fx fxVar, OnTurnBasedMatchUpdateReceivedListener onTurnBasedMatchUpdateReceivedListener, TurnBasedMatch turnBasedMatch) {
            this.GJ = fxVar;
            super(fxVar, onTurnBasedMatchUpdateReceivedListener);
            this.GX = turnBasedMatch;
        }

        protected /* synthetic */ void a(Object obj) {
            b((OnTurnBasedMatchUpdateReceivedListener) obj);
        }

        protected void b(OnTurnBasedMatchUpdateReceivedListener onTurnBasedMatchUpdateReceivedListener) {
            onTurnBasedMatchUpdateReceivedListener.onTurnBasedMatchReceived(this.GX);
        }

        protected void cP() {
        }
    }

    final class z extends b<RealTimeMessageReceivedListener> {
        final /* synthetic */ fx GJ;
        private final RealTimeMessage GY;

        z(fx fxVar, RealTimeMessageReceivedListener realTimeMessageReceivedListener, RealTimeMessage realTimeMessage) {
            this.GJ = fxVar;
            super(fxVar, realTimeMessageReceivedListener);
            this.GY = realTimeMessage;
        }

        public void a(RealTimeMessageReceivedListener realTimeMessageReceivedListener) {
            if (realTimeMessageReceivedListener != null) {
                realTimeMessageReceivedListener.onRealTimeMessageReceived(this.GY);
            }
        }

        protected void cP() {
        }
    }

    final class aj extends com.google.android.gms.internal.eh.d<com.google.android.gms.common.api.a.c<LoadPlayerScoreResult>> implements LoadPlayerScoreResult {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.games.leaderboard.d Ha;
        private final Status vl;

        aj(fx fxVar, com.google.android.gms.common.api.a.c<LoadPlayerScoreResult> cVar, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, cVar, dataHolder);
            this.vl = new Status(dataHolder.getStatusCode());
            LeaderboardScoreBuffer leaderboardScoreBuffer = new LeaderboardScoreBuffer(dataHolder);
            try {
                if (leaderboardScoreBuffer.getCount() > 0) {
                    this.Ha = (com.google.android.gms.games.leaderboard.d) leaderboardScoreBuffer.get(0).freeze();
                } else {
                    this.Ha = null;
                }
                leaderboardScoreBuffer.close();
            } catch (Throwable th) {
                leaderboardScoreBuffer.close();
            }
        }

        protected void a(com.google.android.gms.common.api.a.c<LoadPlayerScoreResult> cVar, DataHolder dataHolder) {
            cVar.b(this);
        }

        public LeaderboardScore getScore() {
            return this.Ha;
        }

        public Status getStatus() {
            return this.vl;
        }
    }

    final class as extends b<com.google.android.gms.common.api.a.c<LoadRequestsResult>> implements LoadRequestsResult {
        final /* synthetic */ fx GJ;
        private final Bundle Hj;
        private final Status vl;

        as(fx fxVar, com.google.android.gms.common.api.a.c<LoadRequestsResult> cVar, Status status, Bundle bundle) {
            this.GJ = fxVar;
            super(fxVar, cVar);
            this.vl = status;
            this.Hj = bundle;
        }

        protected /* synthetic */ void a(Object obj) {
            c((com.google.android.gms.common.api.a.c) obj);
        }

        protected void c(com.google.android.gms.common.api.a.c<LoadRequestsResult> cVar) {
            cVar.b(this);
        }

        protected void cP() {
            release();
        }

        public GameRequestBuffer getRequests(int requestType) {
            String aW = gs.aW(requestType);
            return !this.Hj.containsKey(aW) ? null : new GameRequestBuffer((DataHolder) this.Hj.get(aW));
        }

        public Status getStatus() {
            return this.vl;
        }

        public void release() {
            for (String parcelable : this.Hj.keySet()) {
                DataHolder dataHolder = (DataHolder) this.Hj.getParcelable(parcelable);
                if (dataHolder != null) {
                    dataHolder.close();
                }
            }
        }
    }

    abstract class av<R extends com.google.android.gms.common.api.a.c<?>> extends com.google.android.gms.internal.eh.d<R> implements Releasable, Result {
        final /* synthetic */ fx GJ;
        final Status vl;
        final DataHolder zU;

        public av(fx fxVar, R r, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, r, dataHolder);
            this.vl = new Status(dataHolder.getStatusCode());
            this.zU = dataHolder;
        }

        public Status getStatus() {
            return this.vl;
        }

        public void release() {
            if (this.zU != null) {
                this.zU.close();
            }
        }
    }

    abstract class b extends com.google.android.gms.internal.eh.d<RoomUpdateListener> {
        final /* synthetic */ fx GJ;

        b(fx fxVar, RoomUpdateListener roomUpdateListener, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, roomUpdateListener, dataHolder);
        }

        protected void a(RoomUpdateListener roomUpdateListener, DataHolder dataHolder) {
            a(roomUpdateListener, this.GJ.G(dataHolder), dataHolder.getStatusCode());
        }

        protected abstract void a(RoomUpdateListener roomUpdateListener, Room room, int i);
    }

    final class bh extends b<com.google.android.gms.common.api.a.c<CancelMatchResult>> implements CancelMatchResult {
        final /* synthetic */ fx GJ;
        private final String Hr;
        private final Status vl;

        bh(fx fxVar, com.google.android.gms.common.api.a.c<CancelMatchResult> cVar, Status status, String str) {
            this.GJ = fxVar;
            super(fxVar, cVar);
            this.vl = status;
            this.Hr = str;
        }

        public /* synthetic */ void a(Object obj) {
            c((com.google.android.gms.common.api.a.c) obj);
        }

        public void c(com.google.android.gms.common.api.a.c<CancelMatchResult> cVar) {
            cVar.b(this);
        }

        protected void cP() {
        }

        public String getMatchId() {
            return this.Hr;
        }

        public Status getStatus() {
            return this.vl;
        }
    }

    final class br extends b<com.google.android.gms.common.api.a.c<LoadMatchesResult>> implements LoadMatchesResult {
        final /* synthetic */ fx GJ;
        private final LoadMatchesResponse Hx;
        private final Status vl;

        br(fx fxVar, com.google.android.gms.common.api.a.c<LoadMatchesResult> cVar, Status status, Bundle bundle) {
            this.GJ = fxVar;
            super(fxVar, cVar);
            this.vl = status;
            this.Hx = new LoadMatchesResponse(bundle);
        }

        protected /* synthetic */ void a(Object obj) {
            c((com.google.android.gms.common.api.a.c) obj);
        }

        protected void c(com.google.android.gms.common.api.a.c<LoadMatchesResult> cVar) {
            cVar.b(this);
        }

        protected void cP() {
        }

        public LoadMatchesResponse getMatches() {
            return this.Hx;
        }

        public Status getStatus() {
            return this.vl;
        }

        public void release() {
            this.Hx.close();
        }
    }

    abstract class c extends com.google.android.gms.internal.eh.d<RoomStatusUpdateListener> {
        final /* synthetic */ fx GJ;

        c(fx fxVar, RoomStatusUpdateListener roomStatusUpdateListener, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, roomStatusUpdateListener, dataHolder);
        }

        protected void a(RoomStatusUpdateListener roomStatusUpdateListener, DataHolder dataHolder) {
            a(roomStatusUpdateListener, this.GJ.G(dataHolder));
        }

        protected abstract void a(RoomStatusUpdateListener roomStatusUpdateListener, Room room);
    }

    final class e extends b<com.google.android.gms.common.api.a.c<UpdateAchievementResult>> implements UpdateAchievementResult {
        final /* synthetic */ fx GJ;
        private final String GK;
        private final Status vl;

        e(fx fxVar, com.google.android.gms.common.api.a.c<UpdateAchievementResult> cVar, int i, String str) {
            this.GJ = fxVar;
            super(fxVar, cVar);
            this.vl = new Status(i);
            this.GK = str;
        }

        protected /* synthetic */ void a(Object obj) {
            c((com.google.android.gms.common.api.a.c) obj);
        }

        protected void c(com.google.android.gms.common.api.a.c<UpdateAchievementResult> cVar) {
            cVar.b(this);
        }

        protected void cP() {
        }

        public String getAchievementId() {
            return this.GK;
        }

        public Status getStatus() {
            return this.vl;
        }
    }

    abstract class a extends c {
        private final ArrayList<String> GI;
        final /* synthetic */ fx GJ;

        a(fx fxVar, RoomStatusUpdateListener roomStatusUpdateListener, DataHolder dataHolder, String[] strArr) {
            this.GJ = fxVar;
            super(fxVar, roomStatusUpdateListener, dataHolder);
            this.GI = new ArrayList();
            for (Object add : strArr) {
                this.GI.add(add);
            }
        }

        protected void a(RoomStatusUpdateListener roomStatusUpdateListener, Room room) {
            a(roomStatusUpdateListener, room, this.GI);
        }

        protected abstract void a(RoomStatusUpdateListener roomStatusUpdateListener, Room room, ArrayList<String> arrayList);
    }

    final class ai extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<LoadPlayerScoreResult> vj;

        ai(fx fxVar, com.google.android.gms.common.api.a.c<LoadPlayerScoreResult> cVar) {
            this.GJ = fxVar;
            this.vj = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void C(DataHolder dataHolder) {
            this.GJ.a(new aj(this.GJ, this.vj, dataHolder));
        }
    }

    final class ak extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<LoadPlayersResult> vj;

        ak(fx fxVar, com.google.android.gms.common.api.a.c<LoadPlayersResult> cVar) {
            this.GJ = fxVar;
            this.vj = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void e(DataHolder dataHolder) {
            this.GJ.a(new al(this.GJ, this.vj, dataHolder));
        }
    }

    final class al extends av<com.google.android.gms.common.api.a.c<LoadPlayersResult>> implements LoadPlayersResult {
        final /* synthetic */ fx GJ;
        private final PlayerBuffer Hb;

        al(fx fxVar, com.google.android.gms.common.api.a.c<LoadPlayersResult> cVar, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, cVar, dataHolder);
            this.Hb = new PlayerBuffer(dataHolder);
        }

        protected void a(com.google.android.gms.common.api.a.c<LoadPlayersResult> cVar, DataHolder dataHolder) {
            cVar.b(this);
        }

        public PlayerBuffer getPlayers() {
            return this.Hb;
        }
    }

    final class an extends fw {
        final /* synthetic */ fx GJ;
        final ReliableMessageSentCallback He;

        public an(fx fxVar, ReliableMessageSentCallback reliableMessageSentCallback) {
            this.GJ = fxVar;
            this.He = reliableMessageSentCallback;
        }

        public void b(int i, int i2, String str) {
            this.GJ.a(new am(this.GJ, this.He, i, i2, str));
        }
    }

    final class ao extends fw {
        final /* synthetic */ fx GJ;
        private final OnRequestReceivedListener Hf;

        ao(fx fxVar, OnRequestReceivedListener onRequestReceivedListener) {
            this.GJ = fxVar;
            this.Hf = onRequestReceivedListener;
        }

        public void m(DataHolder dataHolder) {
            GameRequestBuffer gameRequestBuffer = new GameRequestBuffer(dataHolder);
            GameRequest gameRequest = null;
            try {
                if (gameRequestBuffer.getCount() > 0) {
                    gameRequest = (GameRequest) ((GameRequest) gameRequestBuffer.get(0)).freeze();
                }
                gameRequestBuffer.close();
                if (gameRequest != null) {
                    this.GJ.a(new ap(this.GJ, this.Hf, gameRequest));
                }
            } catch (Throwable th) {
                gameRequestBuffer.close();
            }
        }

        public void onRequestRemoved(String requestId) {
            this.GJ.a(new aq(this.GJ, this.Hf, requestId));
        }
    }

    final class ar extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<LoadRequestsResult> Hi;

        public ar(fx fxVar, com.google.android.gms.common.api.a.c<LoadRequestsResult> cVar) {
            this.GJ = fxVar;
            this.Hi = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void b(int i, Bundle bundle) {
            bundle.setClassLoader(getClass().getClassLoader());
            this.GJ.a(new as(this.GJ, this.Hi, new Status(i), bundle));
        }
    }

    final class at extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<UpdateRequestsResult> Hk;

        public at(fx fxVar, com.google.android.gms.common.api.a.c<UpdateRequestsResult> cVar) {
            this.GJ = fxVar;
            this.Hk = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void D(DataHolder dataHolder) {
            this.GJ.a(new au(this.GJ, this.Hk, dataHolder));
        }
    }

    final class au extends av<com.google.android.gms.common.api.a.c<UpdateRequestsResult>> implements UpdateRequestsResult {
        final /* synthetic */ fx GJ;
        private final hb Hl;

        au(fx fxVar, com.google.android.gms.common.api.a.c<UpdateRequestsResult> cVar, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, cVar, dataHolder);
            this.Hl = hb.H(dataHolder);
        }

        protected void a(com.google.android.gms.common.api.a.c<UpdateRequestsResult> cVar, DataHolder dataHolder) {
            cVar.b(this);
        }

        public Set<String> getRequestIds() {
            return this.Hl.getRequestIds();
        }

        public int getRequestOutcome(String requestId) {
            return this.Hl.getRequestOutcome(requestId);
        }
    }

    final class aw extends c {
        final /* synthetic */ fx GJ;

        aw(fx fxVar, RoomStatusUpdateListener roomStatusUpdateListener, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, roomStatusUpdateListener, dataHolder);
        }

        public void a(RoomStatusUpdateListener roomStatusUpdateListener, Room room) {
            roomStatusUpdateListener.onRoomAutoMatching(room);
        }
    }

    final class ax extends fw {
        final /* synthetic */ fx GJ;
        private final RoomUpdateListener Hm;
        private final RoomStatusUpdateListener Hn;
        private final RealTimeMessageReceivedListener Ho;

        public ax(fx fxVar, RoomUpdateListener roomUpdateListener) {
            this.GJ = fxVar;
            this.Hm = (RoomUpdateListener) er.b((Object) roomUpdateListener, (Object) "Callbacks must not be null");
            this.Hn = null;
            this.Ho = null;
        }

        public ax(fx fxVar, RoomUpdateListener roomUpdateListener, RoomStatusUpdateListener roomStatusUpdateListener, RealTimeMessageReceivedListener realTimeMessageReceivedListener) {
            this.GJ = fxVar;
            this.Hm = (RoomUpdateListener) er.b((Object) roomUpdateListener, (Object) "Callbacks must not be null");
            this.Hn = roomStatusUpdateListener;
            this.Ho = realTimeMessageReceivedListener;
        }

        public void a(DataHolder dataHolder, String[] strArr) {
            this.GJ.a(new af(this.GJ, this.Hn, dataHolder, strArr));
        }

        public void b(DataHolder dataHolder, String[] strArr) {
            this.GJ.a(new ag(this.GJ, this.Hn, dataHolder, strArr));
        }

        public void c(DataHolder dataHolder, String[] strArr) {
            this.GJ.a(new ah(this.GJ, this.Hn, dataHolder, strArr));
        }

        public void d(DataHolder dataHolder, String[] strArr) {
            this.GJ.a(new ad(this.GJ, this.Hn, dataHolder, strArr));
        }

        public void e(DataHolder dataHolder, String[] strArr) {
            this.GJ.a(new ac(this.GJ, this.Hn, dataHolder, strArr));
        }

        public void f(DataHolder dataHolder, String[] strArr) {
            this.GJ.a(new ae(this.GJ, this.Hn, dataHolder, strArr));
        }

        public void onLeftRoom(int statusCode, String externalRoomId) {
            this.GJ.a(new v(this.GJ, this.Hm, statusCode, externalRoomId));
        }

        public void onP2PConnected(String participantId) {
            this.GJ.a(new aa(this.GJ, this.Hn, participantId));
        }

        public void onP2PDisconnected(String participantId) {
            this.GJ.a(new ab(this.GJ, this.Hn, participantId));
        }

        public void onRealTimeMessageReceived(RealTimeMessage message) {
            this.GJ.a(new z(this.GJ, this.Ho, message));
        }

        public void s(DataHolder dataHolder) {
            this.GJ.a(new ba(this.GJ, this.Hm, dataHolder));
        }

        public void t(DataHolder dataHolder) {
            this.GJ.a(new q(this.GJ, this.Hm, dataHolder));
        }

        public void u(DataHolder dataHolder) {
            this.GJ.a(new az(this.GJ, this.Hn, dataHolder));
        }

        public void v(DataHolder dataHolder) {
            this.GJ.a(new aw(this.GJ, this.Hn, dataHolder));
        }

        public void w(DataHolder dataHolder) {
            this.GJ.a(new ay(this.GJ, this.Hm, dataHolder));
        }

        public void x(DataHolder dataHolder) {
            this.GJ.a(new h(this.GJ, this.Hn, dataHolder));
        }

        public void y(DataHolder dataHolder) {
            this.GJ.a(new i(this.GJ, this.Hn, dataHolder));
        }
    }

    final class ay extends b {
        final /* synthetic */ fx GJ;

        ay(fx fxVar, RoomUpdateListener roomUpdateListener, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, roomUpdateListener, dataHolder);
        }

        public void a(RoomUpdateListener roomUpdateListener, Room room, int i) {
            roomUpdateListener.onRoomConnected(i, room);
        }
    }

    final class az extends c {
        final /* synthetic */ fx GJ;

        az(fx fxVar, RoomStatusUpdateListener roomStatusUpdateListener, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, roomStatusUpdateListener, dataHolder);
        }

        public void a(RoomStatusUpdateListener roomStatusUpdateListener, Room room) {
            roomStatusUpdateListener.onRoomConnecting(room);
        }
    }

    final class ba extends b {
        final /* synthetic */ fx GJ;

        public ba(fx fxVar, RoomUpdateListener roomUpdateListener, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, roomUpdateListener, dataHolder);
        }

        public void a(RoomUpdateListener roomUpdateListener, Room room, int i) {
            roomUpdateListener.onRoomCreated(i, room);
        }
    }

    final class bb extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<Status> vj;

        public bb(fx fxVar, com.google.android.gms.common.api.a.c<Status> cVar) {
            this.GJ = fxVar;
            this.vj = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void cM() {
            this.GJ.a(new bc(this.GJ, this.vj, new Status(0)));
        }
    }

    final class bd extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<SubmitScoreResult> vj;

        public bd(fx fxVar, com.google.android.gms.common.api.a.c<SubmitScoreResult> cVar) {
            this.GJ = fxVar;
            this.vj = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void d(DataHolder dataHolder) {
            this.GJ.a(new be(this.GJ, this.vj, dataHolder));
        }
    }

    final class be extends av<com.google.android.gms.common.api.a.c<SubmitScoreResult>> implements SubmitScoreResult {
        final /* synthetic */ fx GJ;
        private final ScoreSubmissionData Hp;

        public be(fx fxVar, com.google.android.gms.common.api.a.c<SubmitScoreResult> cVar, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, cVar, dataHolder);
            try {
                this.Hp = new ScoreSubmissionData(dataHolder);
            } finally {
                dataHolder.close();
            }
        }

        public void a(com.google.android.gms.common.api.a.c<SubmitScoreResult> cVar, DataHolder dataHolder) {
            cVar.b(this);
        }

        public ScoreSubmissionData getScoreData() {
            return this.Hp;
        }
    }

    abstract class bf<T extends com.google.android.gms.common.api.a.c<?>> extends av<T> {
        final /* synthetic */ fx GJ;
        final TurnBasedMatch GX;

        bf(fx fxVar, T t, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, t, dataHolder);
            TurnBasedMatchBuffer turnBasedMatchBuffer = new TurnBasedMatchBuffer(dataHolder);
            try {
                if (turnBasedMatchBuffer.getCount() > 0) {
                    this.GX = (TurnBasedMatch) ((TurnBasedMatch) turnBasedMatchBuffer.get(0)).freeze();
                } else {
                    this.GX = null;
                }
                turnBasedMatchBuffer.close();
            } catch (Throwable th) {
                turnBasedMatchBuffer.close();
            }
        }

        protected void a(T t, DataHolder dataHolder) {
            h(t);
        }

        public TurnBasedMatch getMatch() {
            return this.GX;
        }

        abstract void h(T t);
    }

    final class bg extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<CancelMatchResult> Hq;

        public bg(fx fxVar, com.google.android.gms.common.api.a.c<CancelMatchResult> cVar) {
            this.GJ = fxVar;
            this.Hq = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void f(int i, String str) {
            this.GJ.a(new bh(this.GJ, this.Hq, new Status(i), str));
        }
    }

    final class bi extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<InitiateMatchResult> Hs;

        public bi(fx fxVar, com.google.android.gms.common.api.a.c<InitiateMatchResult> cVar) {
            this.GJ = fxVar;
            this.Hs = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void o(DataHolder dataHolder) {
            this.GJ.a(new bj(this.GJ, this.Hs, dataHolder));
        }
    }

    final class bk extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<LeaveMatchResult> Ht;

        public bk(fx fxVar, com.google.android.gms.common.api.a.c<LeaveMatchResult> cVar) {
            this.GJ = fxVar;
            this.Ht = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void q(DataHolder dataHolder) {
            this.GJ.a(new bl(this.GJ, this.Ht, dataHolder));
        }
    }

    final class bm extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<LoadMatchResult> Hu;

        public bm(fx fxVar, com.google.android.gms.common.api.a.c<LoadMatchResult> cVar) {
            this.GJ = fxVar;
            this.Hu = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void n(DataHolder dataHolder) {
            this.GJ.a(new bn(this.GJ, this.Hu, dataHolder));
        }
    }

    final class bo extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<UpdateMatchResult> Hv;

        public bo(fx fxVar, com.google.android.gms.common.api.a.c<UpdateMatchResult> cVar) {
            this.GJ = fxVar;
            this.Hv = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void p(DataHolder dataHolder) {
            this.GJ.a(new bp(this.GJ, this.Hv, dataHolder));
        }
    }

    final class bq extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<LoadMatchesResult> Hw;

        public bq(fx fxVar, com.google.android.gms.common.api.a.c<LoadMatchesResult> cVar) {
            this.GJ = fxVar;
            this.Hw = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void a(int i, Bundle bundle) {
            bundle.setClassLoader(getClass().getClassLoader());
            this.GJ.a(new br(this.GJ, this.Hw, new Status(i), bundle));
        }
    }

    final class d extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<UpdateAchievementResult> vj;

        d(fx fxVar, com.google.android.gms.common.api.a.c<UpdateAchievementResult> cVar) {
            this.GJ = fxVar;
            this.vj = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void e(int i, String str) {
            this.GJ.a(new e(this.GJ, this.vj, i, str));
        }
    }

    final class f extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<LoadAchievementsResult> vj;

        f(fx fxVar, com.google.android.gms.common.api.a.c<LoadAchievementsResult> cVar) {
            this.GJ = fxVar;
            this.vj = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void b(DataHolder dataHolder) {
            this.GJ.a(new g(this.GJ, this.vj, dataHolder));
        }
    }

    final class g extends av<com.google.android.gms.common.api.a.c<LoadAchievementsResult>> implements LoadAchievementsResult {
        final /* synthetic */ fx GJ;
        private final AchievementBuffer GL;

        g(fx fxVar, com.google.android.gms.common.api.a.c<LoadAchievementsResult> cVar, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, cVar, dataHolder);
            this.GL = new AchievementBuffer(dataHolder);
        }

        protected void a(com.google.android.gms.common.api.a.c<LoadAchievementsResult> cVar, DataHolder dataHolder) {
            cVar.b(this);
        }

        public AchievementBuffer getAchievements() {
            return this.GL;
        }
    }

    final class h extends c {
        final /* synthetic */ fx GJ;

        h(fx fxVar, RoomStatusUpdateListener roomStatusUpdateListener, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, roomStatusUpdateListener, dataHolder);
        }

        public void a(RoomStatusUpdateListener roomStatusUpdateListener, Room room) {
            roomStatusUpdateListener.onConnectedToRoom(room);
        }
    }

    final class i extends c {
        final /* synthetic */ fx GJ;

        i(fx fxVar, RoomStatusUpdateListener roomStatusUpdateListener, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, roomStatusUpdateListener, dataHolder);
        }

        public void a(RoomStatusUpdateListener roomStatusUpdateListener, Room room) {
            roomStatusUpdateListener.onDisconnectedFromRoom(room);
        }
    }

    final class j extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<LoadGamesResult> vj;

        j(fx fxVar, com.google.android.gms.common.api.a.c<LoadGamesResult> cVar) {
            this.GJ = fxVar;
            this.vj = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void g(DataHolder dataHolder) {
            this.GJ.a(new k(this.GJ, this.vj, dataHolder));
        }
    }

    final class k extends av<com.google.android.gms.common.api.a.c<LoadGamesResult>> implements LoadGamesResult {
        final /* synthetic */ fx GJ;
        private final GameBuffer GM;

        k(fx fxVar, com.google.android.gms.common.api.a.c<LoadGamesResult> cVar, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, cVar, dataHolder);
            this.GM = new GameBuffer(dataHolder);
        }

        protected void a(com.google.android.gms.common.api.a.c<LoadGamesResult> cVar, DataHolder dataHolder) {
            cVar.b(this);
        }

        public GameBuffer getGames() {
            return this.GM;
        }
    }

    final class l extends fw {
        final /* synthetic */ fx GJ;
        private final OnInvitationReceivedListener GN;

        l(fx fxVar, OnInvitationReceivedListener onInvitationReceivedListener) {
            this.GJ = fxVar;
            this.GN = onInvitationReceivedListener;
        }

        public void l(DataHolder dataHolder) {
            InvitationBuffer invitationBuffer = new InvitationBuffer(dataHolder);
            Invitation invitation = null;
            try {
                if (invitationBuffer.getCount() > 0) {
                    invitation = (Invitation) ((Invitation) invitationBuffer.get(0)).freeze();
                }
                invitationBuffer.close();
                if (invitation != null) {
                    this.GJ.a(new m(this.GJ, this.GN, invitation));
                }
            } catch (Throwable th) {
                invitationBuffer.close();
            }
        }

        public void onInvitationRemoved(String invitationId) {
            this.GJ.a(new n(this.GJ, this.GN, invitationId));
        }
    }

    final class o extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<LoadInvitationsResult> vj;

        o(fx fxVar, com.google.android.gms.common.api.a.c<LoadInvitationsResult> cVar) {
            this.GJ = fxVar;
            this.vj = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void k(DataHolder dataHolder) {
            this.GJ.a(new p(this.GJ, this.vj, dataHolder));
        }
    }

    final class p extends av<com.google.android.gms.common.api.a.c<LoadInvitationsResult>> implements LoadInvitationsResult {
        final /* synthetic */ fx GJ;
        private final InvitationBuffer GQ;

        p(fx fxVar, com.google.android.gms.common.api.a.c<LoadInvitationsResult> cVar, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, cVar, dataHolder);
            this.GQ = new InvitationBuffer(dataHolder);
        }

        protected void a(com.google.android.gms.common.api.a.c<LoadInvitationsResult> cVar, DataHolder dataHolder) {
            cVar.b(this);
        }

        public InvitationBuffer getInvitations() {
            return this.GQ;
        }
    }

    final class q extends b {
        final /* synthetic */ fx GJ;

        public q(fx fxVar, RoomUpdateListener roomUpdateListener, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, roomUpdateListener, dataHolder);
        }

        public void a(RoomUpdateListener roomUpdateListener, Room room, int i) {
            roomUpdateListener.onJoinedRoom(i, room);
        }
    }

    final class r extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<LoadScoresResult> vj;

        r(fx fxVar, com.google.android.gms.common.api.a.c<LoadScoresResult> cVar) {
            this.GJ = fxVar;
            this.vj = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void a(DataHolder dataHolder, DataHolder dataHolder2) {
            this.GJ.a(new s(this.GJ, this.vj, dataHolder, dataHolder2));
        }
    }

    final class s extends av<com.google.android.gms.common.api.a.c<LoadScoresResult>> implements LoadScoresResult {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.games.leaderboard.a GR;
        private final LeaderboardScoreBuffer GS;

        s(fx fxVar, com.google.android.gms.common.api.a.c<LoadScoresResult> cVar, DataHolder dataHolder, DataHolder dataHolder2) {
            this.GJ = fxVar;
            super(fxVar, cVar, dataHolder2);
            LeaderboardBuffer leaderboardBuffer = new LeaderboardBuffer(dataHolder);
            try {
                if (leaderboardBuffer.getCount() > 0) {
                    this.GR = (com.google.android.gms.games.leaderboard.a) ((Leaderboard) leaderboardBuffer.get(0)).freeze();
                } else {
                    this.GR = null;
                }
                leaderboardBuffer.close();
                this.GS = new LeaderboardScoreBuffer(dataHolder2);
            } catch (Throwable th) {
                leaderboardBuffer.close();
            }
        }

        protected void a(com.google.android.gms.common.api.a.c<LoadScoresResult> cVar, DataHolder dataHolder) {
            cVar.b(this);
        }

        public Leaderboard getLeaderboard() {
            return this.GR;
        }

        public LeaderboardScoreBuffer getScores() {
            return this.GS;
        }
    }

    final class t extends fw {
        final /* synthetic */ fx GJ;
        private final com.google.android.gms.common.api.a.c<LeaderboardMetadataResult> vj;

        t(fx fxVar, com.google.android.gms.common.api.a.c<LeaderboardMetadataResult> cVar) {
            this.GJ = fxVar;
            this.vj = (com.google.android.gms.common.api.a.c) er.b((Object) cVar, (Object) "Holder must not be null");
        }

        public void c(DataHolder dataHolder) {
            this.GJ.a(new u(this.GJ, this.vj, dataHolder));
        }
    }

    final class u extends av<com.google.android.gms.common.api.a.c<LeaderboardMetadataResult>> implements LeaderboardMetadataResult {
        final /* synthetic */ fx GJ;
        private final LeaderboardBuffer GT;

        u(fx fxVar, com.google.android.gms.common.api.a.c<LeaderboardMetadataResult> cVar, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, cVar, dataHolder);
            this.GT = new LeaderboardBuffer(dataHolder);
        }

        protected void a(com.google.android.gms.common.api.a.c<LeaderboardMetadataResult> cVar, DataHolder dataHolder) {
            cVar.b(this);
        }

        public LeaderboardBuffer getLeaderboards() {
            return this.GT;
        }
    }

    final class x extends fw {
        final /* synthetic */ fx GJ;
        private final OnTurnBasedMatchUpdateReceivedListener GW;

        x(fx fxVar, OnTurnBasedMatchUpdateReceivedListener onTurnBasedMatchUpdateReceivedListener) {
            this.GJ = fxVar;
            this.GW = onTurnBasedMatchUpdateReceivedListener;
        }

        public void onTurnBasedMatchRemoved(String matchId) {
            this.GJ.a(new w(this.GJ, this.GW, matchId));
        }

        public void r(DataHolder dataHolder) {
            TurnBasedMatchBuffer turnBasedMatchBuffer = new TurnBasedMatchBuffer(dataHolder);
            TurnBasedMatch turnBasedMatch = null;
            try {
                if (turnBasedMatchBuffer.getCount() > 0) {
                    turnBasedMatch = (TurnBasedMatch) ((TurnBasedMatch) turnBasedMatchBuffer.get(0)).freeze();
                }
                turnBasedMatchBuffer.close();
                if (turnBasedMatch != null) {
                    this.GJ.a(new y(this.GJ, this.GW, turnBasedMatch));
                }
            } catch (Throwable th) {
                turnBasedMatchBuffer.close();
            }
        }
    }

    final class ac extends a {
        final /* synthetic */ fx GJ;

        ac(fx fxVar, RoomStatusUpdateListener roomStatusUpdateListener, DataHolder dataHolder, String[] strArr) {
            this.GJ = fxVar;
            super(fxVar, roomStatusUpdateListener, dataHolder, strArr);
        }

        protected void a(RoomStatusUpdateListener roomStatusUpdateListener, Room room, ArrayList<String> arrayList) {
            roomStatusUpdateListener.onPeersConnected(room, arrayList);
        }
    }

    final class ad extends a {
        final /* synthetic */ fx GJ;

        ad(fx fxVar, RoomStatusUpdateListener roomStatusUpdateListener, DataHolder dataHolder, String[] strArr) {
            this.GJ = fxVar;
            super(fxVar, roomStatusUpdateListener, dataHolder, strArr);
        }

        protected void a(RoomStatusUpdateListener roomStatusUpdateListener, Room room, ArrayList<String> arrayList) {
            roomStatusUpdateListener.onPeerDeclined(room, arrayList);
        }
    }

    final class ae extends a {
        final /* synthetic */ fx GJ;

        ae(fx fxVar, RoomStatusUpdateListener roomStatusUpdateListener, DataHolder dataHolder, String[] strArr) {
            this.GJ = fxVar;
            super(fxVar, roomStatusUpdateListener, dataHolder, strArr);
        }

        protected void a(RoomStatusUpdateListener roomStatusUpdateListener, Room room, ArrayList<String> arrayList) {
            roomStatusUpdateListener.onPeersDisconnected(room, arrayList);
        }
    }

    final class af extends a {
        final /* synthetic */ fx GJ;

        af(fx fxVar, RoomStatusUpdateListener roomStatusUpdateListener, DataHolder dataHolder, String[] strArr) {
            this.GJ = fxVar;
            super(fxVar, roomStatusUpdateListener, dataHolder, strArr);
        }

        protected void a(RoomStatusUpdateListener roomStatusUpdateListener, Room room, ArrayList<String> arrayList) {
            roomStatusUpdateListener.onPeerInvitedToRoom(room, arrayList);
        }
    }

    final class ag extends a {
        final /* synthetic */ fx GJ;

        ag(fx fxVar, RoomStatusUpdateListener roomStatusUpdateListener, DataHolder dataHolder, String[] strArr) {
            this.GJ = fxVar;
            super(fxVar, roomStatusUpdateListener, dataHolder, strArr);
        }

        protected void a(RoomStatusUpdateListener roomStatusUpdateListener, Room room, ArrayList<String> arrayList) {
            roomStatusUpdateListener.onPeerJoined(room, arrayList);
        }
    }

    final class ah extends a {
        final /* synthetic */ fx GJ;

        ah(fx fxVar, RoomStatusUpdateListener roomStatusUpdateListener, DataHolder dataHolder, String[] strArr) {
            this.GJ = fxVar;
            super(fxVar, roomStatusUpdateListener, dataHolder, strArr);
        }

        protected void a(RoomStatusUpdateListener roomStatusUpdateListener, Room room, ArrayList<String> arrayList) {
            roomStatusUpdateListener.onPeerLeft(room, arrayList);
        }
    }

    final class bj extends bf<com.google.android.gms.common.api.a.c<InitiateMatchResult>> implements InitiateMatchResult {
        final /* synthetic */ fx GJ;

        bj(fx fxVar, com.google.android.gms.common.api.a.c<InitiateMatchResult> cVar, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, cVar, dataHolder);
        }

        protected void h(com.google.android.gms.common.api.a.c<InitiateMatchResult> cVar) {
            cVar.b(this);
        }
    }

    final class bl extends bf<com.google.android.gms.common.api.a.c<LeaveMatchResult>> implements LeaveMatchResult {
        final /* synthetic */ fx GJ;

        bl(fx fxVar, com.google.android.gms.common.api.a.c<LeaveMatchResult> cVar, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, cVar, dataHolder);
        }

        protected void h(com.google.android.gms.common.api.a.c<LeaveMatchResult> cVar) {
            cVar.b(this);
        }
    }

    final class bn extends bf<com.google.android.gms.common.api.a.c<LoadMatchResult>> implements LoadMatchResult {
        final /* synthetic */ fx GJ;

        bn(fx fxVar, com.google.android.gms.common.api.a.c<LoadMatchResult> cVar, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, cVar, dataHolder);
        }

        protected void h(com.google.android.gms.common.api.a.c<LoadMatchResult> cVar) {
            cVar.b(this);
        }
    }

    final class bp extends bf<com.google.android.gms.common.api.a.c<UpdateMatchResult>> implements UpdateMatchResult {
        final /* synthetic */ fx GJ;

        bp(fx fxVar, com.google.android.gms.common.api.a.c<UpdateMatchResult> cVar, DataHolder dataHolder) {
            this.GJ = fxVar;
            super(fxVar, cVar, dataHolder);
        }

        protected void h(com.google.android.gms.common.api.a.c<UpdateMatchResult> cVar) {
            cVar.b(this);
        }
    }

    public fx(Context context, Looper looper, String str, String str2, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener, String[] strArr, int i, View view, boolean z, boolean z2, int i2, boolean z3, int i3) {
        super(context, looper, connectionCallbacks, onConnectionFailedListener, strArr);
        this.GA = false;
        this.GB = false;
        this.Gv = str;
        this.vi = (String) er.f(str2);
        this.GD = new Binder();
        this.Gw = new HashMap();
        this.Gz = gd.a(this, i);
        e(view);
        this.GB = z2;
        this.GC = i2;
        this.GE = (long) hashCode();
        this.GF = z;
        this.GH = z3;
        this.GG = i3;
        registerConnectionCallbacks((ConnectionCallbacks) this);
        registerConnectionFailedListener((OnConnectionFailedListener) this);
    }

    private Room G(DataHolder dataHolder) {
        com.google.android.gms.games.multiplayer.realtime.a aVar = new com.google.android.gms.games.multiplayer.realtime.a(dataHolder);
        Room room = null;
        try {
            if (aVar.getCount() > 0) {
                room = (Room) ((Room) aVar.get(0)).freeze();
            }
            aVar.close();
            return room;
        } catch (Throwable th) {
            aVar.close();
        }
    }

    private RealTimeSocket aw(String str) {
        try {
            ParcelFileDescriptor aD = ((gb) eb()).aD(str);
            RealTimeSocket gcVar;
            if (aD != null) {
                fz.f("GamesClientImpl", "Created native libjingle socket.");
                gcVar = new gc(aD);
                this.Gw.put(str, gcVar);
                return gcVar;
            }
            fz.f("GamesClientImpl", "Unable to create native libjingle socket, resorting to old socket.");
            String ay = ((gb) eb()).ay(str);
            if (ay == null) {
                return null;
            }
            LocalSocket localSocket = new LocalSocket();
            try {
                localSocket.connect(new LocalSocketAddress(ay));
                gcVar = new ge(localSocket, str);
                this.Gw.put(str, gcVar);
                return gcVar;
            } catch (IOException e) {
                fz.h("GamesClientImpl", "connect() call failed on socket: " + e.getMessage());
                return null;
            }
        } catch (RemoteException e2) {
            fz.h("GamesClientImpl", "Unable to create socket. Service died.");
            return null;
        }
    }

    private void fG() {
        for (RealTimeSocket close : this.Gw.values()) {
            try {
                close.close();
            } catch (Throwable e) {
                fz.a("GamesClientImpl", "IOException:", e);
            }
        }
        this.Gw.clear();
    }

    private void fm() {
        this.Gx = null;
    }

    protected gb H(IBinder iBinder) {
        return com.google.android.gms.internal.gb.a.J(iBinder);
    }

    public int a(ReliableMessageSentCallback reliableMessageSentCallback, byte[] bArr, String str, String str2) {
        try {
            return ((gb) eb()).a(new an(this, reliableMessageSentCallback), bArr, str, str2);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return -1;
        }
    }

    public int a(byte[] bArr, String str, String[] strArr) {
        er.b((Object) strArr, (Object) "Participant IDs must not be null");
        try {
            return ((gb) eb()).b(bArr, str, strArr);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return -1;
        }
    }

    public Intent a(int i, int i2, boolean z) {
        try {
            return ((gb) eb()).a(i, i2, z);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    public Intent a(int i, byte[] bArr, int i2, Bitmap bitmap, String str) {
        try {
            Intent a = ((gb) eb()).a(i, bArr, i2, str);
            er.b((Object) bitmap, (Object) "Must provide a non null icon");
            a.putExtra("com.google.android.gms.games.REQUEST_ITEM_ICON", bitmap);
            return a;
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    public Intent a(Room room, int i) {
        try {
            return ((gb) eb()).a((RoomEntity) room.freeze(), i);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    protected void a(int i, IBinder iBinder, Bundle bundle) {
        if (i == 0 && bundle != null) {
            this.GA = bundle.getBoolean("show_welcome_popup");
        }
        super.a(i, iBinder, bundle);
    }

    public void a(IBinder iBinder, Bundle bundle) {
        if (isConnected()) {
            try {
                ((gb) eb()).a(iBinder, bundle);
            } catch (RemoteException e) {
                fz.g("GamesClientImpl", "service died");
            }
        }
    }

    public void a(com.google.android.gms.common.api.a.c<LoadRequestsResult> cVar, int i, int i2, int i3) {
        try {
            ((gb) eb()).a(new ar(this, cVar), i, i2, i3);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<LoadPlayersResult> cVar, int i, boolean z, boolean z2) {
        try {
            ((gb) eb()).a(new ak(this, cVar), i, z, z2);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<LoadMatchesResult> cVar, int i, int[] iArr) {
        try {
            ((gb) eb()).a(new bq(this, cVar), i, iArr);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<LoadScoresResult> cVar, LeaderboardScoreBuffer leaderboardScoreBuffer, int i, int i2) {
        try {
            ((gb) eb()).a(new r(this, cVar), leaderboardScoreBuffer.fX().fY(), i, i2);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<InitiateMatchResult> cVar, TurnBasedMatchConfig turnBasedMatchConfig) {
        try {
            ((gb) eb()).a(new bi(this, cVar), turnBasedMatchConfig.getVariant(), turnBasedMatchConfig.getMinPlayers(), turnBasedMatchConfig.getInvitedPlayerIds(), turnBasedMatchConfig.getAutoMatchCriteria());
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<LoadPlayersResult> cVar, String str) {
        try {
            ((gb) eb()).a(new ak(this, cVar), str);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<UpdateAchievementResult> cVar, String str, int i) {
        try {
            ((gb) eb()).a(cVar == null ? null : new d(this, cVar), str, i, this.Gz.fP(), this.Gz.fO());
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<LoadScoresResult> cVar, String str, int i, int i2, int i3, boolean z) {
        try {
            ((gb) eb()).a(new r(this, cVar), str, i, i2, i3, z);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<LoadPlayersResult> cVar, String str, int i, boolean z, boolean z2) {
        if (str.equals("playedWith")) {
            try {
                ((gb) eb()).d(new ak(this, cVar), str, i, z, z2);
                return;
            } catch (RemoteException e) {
                fz.g("GamesClientImpl", "service died");
                return;
            }
        }
        throw new IllegalArgumentException("Invalid player collection: " + str);
    }

    public void a(com.google.android.gms.common.api.a.c<SubmitScoreResult> cVar, String str, long j, String str2) {
        try {
            ((gb) eb()).a(cVar == null ? null : new bd(this, cVar), str, j, str2);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<LeaveMatchResult> cVar, String str, String str2) {
        try {
            ((gb) eb()).c(new bk(this, cVar), str, str2);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<LoadPlayerScoreResult> cVar, String str, String str2, int i, int i2) {
        try {
            ((gb) eb()).a(new ai(this, cVar), str, str2, i, i2);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<LeaderboardMetadataResult> cVar, String str, boolean z) {
        try {
            ((gb) eb()).c(new t(this, cVar), str, z);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<UpdateMatchResult> cVar, String str, byte[] bArr, String str2, ParticipantResult[] participantResultArr) {
        try {
            ((gb) eb()).a(new bo(this, cVar), str, bArr, str2, participantResultArr);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<UpdateMatchResult> cVar, String str, byte[] bArr, ParticipantResult[] participantResultArr) {
        try {
            ((gb) eb()).a(new bo(this, cVar), str, bArr, participantResultArr);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<LoadPlayersResult> cVar, boolean z) {
        try {
            ((gb) eb()).c(new ak(this, cVar), z);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(com.google.android.gms.common.api.a.c<UpdateRequestsResult> cVar, String[] strArr) {
        try {
            ((gb) eb()).a(new at(this, cVar), strArr);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(OnInvitationReceivedListener onInvitationReceivedListener) {
        try {
            ((gb) eb()).a(new l(this, onInvitationReceivedListener), this.GE);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(RoomConfig roomConfig) {
        try {
            ((gb) eb()).a(new ax(this, roomConfig.getRoomUpdateListener(), roomConfig.getRoomStatusUpdateListener(), roomConfig.getMessageReceivedListener()), this.GD, roomConfig.getVariant(), roomConfig.getInvitedPlayerIds(), roomConfig.getAutoMatchCriteria(), roomConfig.isSocketEnabled(), this.GE);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(RoomUpdateListener roomUpdateListener, String str) {
        try {
            ((gb) eb()).c(new ax(this, roomUpdateListener), str);
            fG();
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(OnTurnBasedMatchUpdateReceivedListener onTurnBasedMatchUpdateReceivedListener) {
        try {
            ((gb) eb()).b(new x(this, onTurnBasedMatchUpdateReceivedListener), this.GE);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void a(OnRequestReceivedListener onRequestReceivedListener) {
        try {
            ((gb) eb()).c(new ao(this, onRequestReceivedListener), this.GE);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    protected void a(en enVar, com.google.android.gms.internal.eh.e eVar) throws RemoteException {
        String locale = getContext().getResources().getConfiguration().locale.toString();
        Bundle bundle = new Bundle();
        bundle.putBoolean("com.google.android.gms.games.key.isHeadless", this.GF);
        bundle.putBoolean("com.google.android.gms.games.key.showConnectingPopup", this.GB);
        bundle.putInt("com.google.android.gms.games.key.connectingPopupGravity", this.GC);
        bundle.putBoolean("com.google.android.gms.games.key.retryingSignIn", this.GH);
        bundle.putInt("com.google.android.gms.games.key.sdkVariant", this.GG);
        enVar.a(eVar, GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_VERSION_CODE, getContext().getPackageName(), this.vi, ea(), this.Gv, this.Gz.fP(), locale, bundle);
    }

    protected String aF() {
        return "com.google.android.gms.games.service.START";
    }

    protected String aG() {
        return "com.google.android.gms.games.internal.IGamesService";
    }

    public void aT(int i) {
        this.Gz.setGravity(i);
    }

    public void aU(int i) {
        try {
            ((gb) eb()).aU(i);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public Intent au(String str) {
        try {
            return ((gb) eb()).au(str);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    public void av(String str) {
        try {
            ((gb) eb()).aC(str);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public Intent b(int i, int i2, boolean z) {
        try {
            return ((gb) eb()).b(i, i2, z);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    public void b(com.google.android.gms.common.api.a.c<Status> cVar) {
        try {
            ((gb) eb()).a(new bb(this, cVar));
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void b(com.google.android.gms.common.api.a.c<UpdateAchievementResult> cVar, String str) {
        if (cVar == null) {
            ga gaVar = null;
        } else {
            Object dVar = new d(this, cVar);
        }
        try {
            ((gb) eb()).a(gaVar, str, this.Gz.fP(), this.Gz.fO());
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void b(com.google.android.gms.common.api.a.c<UpdateAchievementResult> cVar, String str, int i) {
        try {
            ((gb) eb()).b(cVar == null ? null : new d(this, cVar), str, i, this.Gz.fP(), this.Gz.fO());
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void b(com.google.android.gms.common.api.a.c<LoadScoresResult> cVar, String str, int i, int i2, int i3, boolean z) {
        try {
            ((gb) eb()).b(new r(this, cVar), str, i, i2, i3, z);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void b(com.google.android.gms.common.api.a.c<LeaderboardMetadataResult> cVar, boolean z) {
        try {
            ((gb) eb()).b(new t(this, cVar), z);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void b(com.google.android.gms.common.api.a.c<UpdateRequestsResult> cVar, String[] strArr) {
        try {
            ((gb) eb()).b(new at(this, cVar), strArr);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void b(RoomConfig roomConfig) {
        try {
            ((gb) eb()).a(new ax(this, roomConfig.getRoomUpdateListener(), roomConfig.getRoomStatusUpdateListener(), roomConfig.getMessageReceivedListener()), this.GD, roomConfig.getInvitationId(), roomConfig.isSocketEnabled(), this.GE);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    protected void b(String... strArr) {
        int i = 0;
        boolean z = false;
        for (String str : strArr) {
            if (str.equals(Scopes.GAMES)) {
                z = true;
            } else if (str.equals("https://www.googleapis.com/auth/games.firstparty")) {
                i = 1;
            }
        }
        if (i != 0) {
            er.a(!z, String.format("Cannot have both %s and %s!", new Object[]{Scopes.GAMES, "https://www.googleapis.com/auth/games.firstparty"}));
            return;
        }
        er.a(z, String.format("Games APIs requires %s to function.", new Object[]{Scopes.GAMES}));
    }

    public void c(com.google.android.gms.common.api.a.c<LoadInvitationsResult> cVar, int i) {
        try {
            ((gb) eb()).a(new o(this, cVar), i);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void c(com.google.android.gms.common.api.a.c<UpdateAchievementResult> cVar, String str) {
        if (cVar == null) {
            ga gaVar = null;
        } else {
            Object dVar = new d(this, cVar);
        }
        try {
            ((gb) eb()).b(gaVar, str, this.Gz.fP(), this.Gz.fO());
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void c(com.google.android.gms.common.api.a.c<LoadAchievementsResult> cVar, boolean z) {
        try {
            ((gb) eb()).a(new f(this, cVar), z);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public Bundle cY() {
        try {
            Bundle cY = ((gb) eb()).cY();
            if (cY == null) {
                return cY;
            }
            cY.setClassLoader(fx.class.getClassLoader());
            return cY;
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    public void connect() {
        fm();
        super.connect();
    }

    public int d(byte[] bArr, String str) {
        try {
            return ((gb) eb()).b(bArr, str, null);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return -1;
        }
    }

    public void d(com.google.android.gms.common.api.a.c<InitiateMatchResult> cVar, String str) {
        try {
            ((gb) eb()).l(new bi(this, cVar), str);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void disconnect() {
        this.GA = false;
        if (isConnected()) {
            try {
                gb gbVar = (gb) eb();
                gbVar.fH();
                gbVar.n(this.GE);
            } catch (RemoteException e) {
                fz.g("GamesClientImpl", "Failed to notify client disconnect.");
            }
        }
        fG();
        super.disconnect();
    }

    public void e(View view) {
        this.Gz.f(view);
    }

    public void e(com.google.android.gms.common.api.a.c<InitiateMatchResult> cVar, String str) {
        try {
            ((gb) eb()).m(new bi(this, cVar), str);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void f(com.google.android.gms.common.api.a.c<LeaveMatchResult> cVar, String str) {
        try {
            ((gb) eb()).o(new bk(this, cVar), str);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public int fA() {
        try {
            return ((gb) eb()).fA();
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return 4368;
        }
    }

    public String fB() {
        try {
            return ((gb) eb()).fB();
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    public int fC() {
        try {
            return ((gb) eb()).fC();
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return 2;
        }
    }

    public Intent fD() {
        try {
            return ((gb) eb()).fD();
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    public int fE() {
        try {
            return ((gb) eb()).fE();
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return 2;
        }
    }

    public int fF() {
        try {
            return ((gb) eb()).fF();
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return 2;
        }
    }

    public void fH() {
        if (isConnected()) {
            try {
                ((gb) eb()).fH();
            } catch (RemoteException e) {
                fz.g("GamesClientImpl", "service died");
            }
        }
    }

    public String fn() {
        try {
            return ((gb) eb()).fn();
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    public String fo() {
        try {
            return ((gb) eb()).fo();
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    public Player fp() {
        PlayerBuffer playerBuffer;
        bm();
        synchronized (this) {
            if (this.Gx == null) {
                try {
                    playerBuffer = new PlayerBuffer(((gb) eb()).fI());
                    if (playerBuffer.getCount() > 0) {
                        this.Gx = (PlayerEntity) playerBuffer.get(0).freeze();
                    }
                    playerBuffer.close();
                } catch (RemoteException e) {
                    fz.g("GamesClientImpl", "service died");
                } catch (Throwable th) {
                    playerBuffer.close();
                }
            }
        }
        return this.Gx;
    }

    public Game fq() {
        bm();
        synchronized (this) {
            if (this.Gy == null) {
                GameBuffer gameBuffer;
                try {
                    gameBuffer = new GameBuffer(((gb) eb()).fK());
                    if (gameBuffer.getCount() > 0) {
                        this.Gy = (GameEntity) gameBuffer.get(0).freeze();
                    }
                    gameBuffer.close();
                } catch (RemoteException e) {
                    fz.g("GamesClientImpl", "service died");
                } catch (Throwable th) {
                    gameBuffer.close();
                }
            }
        }
        return this.Gy;
    }

    public Intent fr() {
        try {
            return ((gb) eb()).fr();
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    public Intent fs() {
        try {
            return ((gb) eb()).fs();
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    public Intent ft() {
        try {
            return ((gb) eb()).ft();
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    public Intent fu() {
        try {
            return ((gb) eb()).fu();
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    public void fv() {
        try {
            ((gb) eb()).o(this.GE);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void fw() {
        try {
            ((gb) eb()).p(this.GE);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void fx() {
        try {
            ((gb) eb()).q(this.GE);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public Intent fy() {
        try {
            return ((gb) eb()).fy();
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    public Intent fz() {
        try {
            return ((gb) eb()).fz();
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
            return null;
        }
    }

    public void g(com.google.android.gms.common.api.a.c<LoadGamesResult> cVar) {
        try {
            ((gb) eb()).d(new j(this, cVar));
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void g(com.google.android.gms.common.api.a.c<CancelMatchResult> cVar, String str) {
        try {
            ((gb) eb()).n(new bg(this, cVar), str);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void h(com.google.android.gms.common.api.a.c<LoadMatchResult> cVar, String str) {
        try {
            ((gb) eb()).p(new bm(this, cVar), str);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public RealTimeSocket i(String str, String str2) {
        if (str2 == null || !ParticipantUtils.aE(str2)) {
            throw new IllegalArgumentException("Bad participant ID");
        }
        RealTimeSocket realTimeSocket = (RealTimeSocket) this.Gw.get(str2);
        return (realTimeSocket == null || realTimeSocket.isClosed()) ? aw(str2) : realTimeSocket;
    }

    public void l(String str, int i) {
        try {
            ((gb) eb()).l(str, i);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void m(String str, int i) {
        try {
            ((gb) eb()).m(str, i);
        } catch (RemoteException e) {
            fz.g("GamesClientImpl", "service died");
        }
    }

    public void onConnected(Bundle connectionHint) {
        if (this.GA) {
            this.Gz.fN();
            this.GA = false;
        }
    }

    public void onConnectionFailed(ConnectionResult result) {
        this.GA = false;
    }

    public void onConnectionSuspended(int cause) {
    }

    protected /* synthetic */ IInterface p(IBinder iBinder) {
        return H(iBinder);
    }
}
