package com.google.android.gms.games.request;

import android.os.Bundle;
import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.games.Game;
import com.google.android.gms.games.GameEntity;
import com.google.android.gms.games.Player;
import com.google.android.gms.games.PlayerEntity;
import com.google.android.gms.internal.ep;
import java.util.ArrayList;
import java.util.Arrays;

public final class GameRequestEntity implements SafeParcelable, GameRequest {
    public static final GameRequestEntityCreator CREATOR;
    private final int AI;
    private final String Hh;
    private final GameEntity Jq;
    private final long Jr;
    private final byte[] Kf;
    private final PlayerEntity Km;
    private final ArrayList<PlayerEntity> Kn;
    private final long Ko;
    private final Bundle Kp;
    private final int wj;

    static {
        CREATOR = new GameRequestEntityCreator();
    }

    GameRequestEntity(int versionCode, GameEntity game, PlayerEntity sender, byte[] data, String requestId, ArrayList<PlayerEntity> recipients, int type, long creationTimestamp, long expirationTimestamp, Bundle recipientStatuses) {
        this.wj = versionCode;
        this.Jq = game;
        this.Km = sender;
        this.Kf = data;
        this.Hh = requestId;
        this.Kn = recipients;
        this.AI = type;
        this.Jr = creationTimestamp;
        this.Ko = expirationTimestamp;
        this.Kp = recipientStatuses;
    }

    public GameRequestEntity(GameRequest request) {
        this.wj = 1;
        this.Jq = new GameEntity(request.getGame());
        this.Km = new PlayerEntity(request.getSender());
        this.Hh = request.getRequestId();
        this.AI = request.getType();
        this.Jr = request.getCreationTimestamp();
        this.Ko = request.getExpirationTimestamp();
        Object data = request.getData();
        if (data == null) {
            this.Kf = null;
        } else {
            this.Kf = new byte[data.length];
            System.arraycopy(data, 0, this.Kf, 0, data.length);
        }
        ArrayList fU = request.fU();
        int size = fU.size();
        this.Kn = new ArrayList(size);
        this.Kp = new Bundle();
        for (int i = 0; i < size; i++) {
            Player player = (Player) ((Player) fU.get(i)).freeze();
            String playerId = player.getPlayerId();
            this.Kn.add((PlayerEntity) player);
            this.Kp.putInt(playerId, request.getRecipientStatus(playerId));
        }
    }

    static int a(GameRequest gameRequest) {
        return ep.hashCode(gameRequest.getGame(), gameRequest.fU(), gameRequest.getRequestId(), gameRequest.getSender(), b(gameRequest), Integer.valueOf(gameRequest.getType()), Long.valueOf(gameRequest.getCreationTimestamp()), Long.valueOf(gameRequest.getExpirationTimestamp()));
    }

    static boolean a(GameRequest gameRequest, Object obj) {
        if (!(obj instanceof GameRequest)) {
            return false;
        }
        if (gameRequest == obj) {
            return true;
        }
        GameRequest gameRequest2 = (GameRequest) obj;
        return ep.equal(gameRequest2.getGame(), gameRequest.getGame()) && ep.equal(gameRequest2.fU(), gameRequest.fU()) && ep.equal(gameRequest2.getRequestId(), gameRequest.getRequestId()) && ep.equal(gameRequest2.getSender(), gameRequest.getSender()) && Arrays.equals(b(gameRequest2), b(gameRequest)) && ep.equal(Integer.valueOf(gameRequest2.getType()), Integer.valueOf(gameRequest.getType())) && ep.equal(Long.valueOf(gameRequest2.getCreationTimestamp()), Long.valueOf(gameRequest.getCreationTimestamp())) && ep.equal(Long.valueOf(gameRequest2.getExpirationTimestamp()), Long.valueOf(gameRequest.getExpirationTimestamp()));
    }

    private static int[] b(GameRequest gameRequest) {
        ArrayList fU = gameRequest.fU();
        int size = fU.size();
        int[] iArr = new int[size];
        for (int i = 0; i < size; i++) {
            iArr[i] = gameRequest.getRecipientStatus(((Player) fU.get(i)).getPlayerId());
        }
        return iArr;
    }

    static String c(GameRequest gameRequest) {
        return ep.e(gameRequest).a("Game", gameRequest.getGame()).a("Sender", gameRequest.getSender()).a("Recipients", gameRequest.fU()).a("Data", gameRequest.getData()).a("RequestId", gameRequest.getRequestId()).a("Type", Integer.valueOf(gameRequest.getType())).a("CreationTimestamp", Long.valueOf(gameRequest.getCreationTimestamp())).a("ExpirationTimestamp", Long.valueOf(gameRequest.getExpirationTimestamp())).toString();
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object obj) {
        return a(this, obj);
    }

    public ArrayList<Player> fU() {
        return new ArrayList(this.Kn);
    }

    public GameRequest freeze() {
        return this;
    }

    public long getCreationTimestamp() {
        return this.Jr;
    }

    public byte[] getData() {
        return this.Kf;
    }

    public long getExpirationTimestamp() {
        return this.Ko;
    }

    public Game getGame() {
        return this.Jq;
    }

    public Player getRecipient() {
        return this.Kn.isEmpty() ? null : (Player) this.Kn.get(0);
    }

    public int getRecipientStatus() {
        return this.Kp.getInt(((PlayerEntity) this.Kn.get(0)).getPlayerId(), 0);
    }

    public int getRecipientStatus(String playerId) {
        return this.Kp.getInt(playerId, 0);
    }

    public String getRequestId() {
        return this.Hh;
    }

    public Player getSender() {
        return this.Km;
    }

    public int getType() {
        return this.AI;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public Bundle gf() {
        return this.Kp;
    }

    public int hashCode() {
        return a(this);
    }

    public boolean isConsumed() {
        return getRecipientStatus() == 1;
    }

    public boolean isDataValid() {
        return true;
    }

    public String toString() {
        return c(this);
    }

    public void writeToParcel(Parcel dest, int flags) {
        GameRequestEntityCreator.a(this, dest, flags);
    }
}
