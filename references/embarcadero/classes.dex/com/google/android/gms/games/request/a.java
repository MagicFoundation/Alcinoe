package com.google.android.gms.games.request;

import android.os.Parcel;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.common.data.b;
import com.google.android.gms.games.Game;
import com.google.android.gms.games.Player;
import com.google.android.gms.games.d;
import java.util.ArrayList;

public final class a extends b implements GameRequest {
    private final int IN;

    public a(DataHolder dataHolder, int i, int i2) {
        super(dataHolder, i);
        this.IN = i2;
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object obj) {
        return GameRequestEntity.a(this, obj);
    }

    public ArrayList<Player> fU() {
        ArrayList<Player> arrayList = new ArrayList(this.IN);
        for (int i = 0; i < this.IN; i++) {
            arrayList.add(new d(this.zU, this.zW + i, "recipient_"));
        }
        return arrayList;
    }

    public GameRequest freeze() {
        return new GameRequestEntity(this);
    }

    public long getCreationTimestamp() {
        return getLong("creation_timestamp");
    }

    public byte[] getData() {
        return getByteArray("data");
    }

    public long getExpirationTimestamp() {
        return getLong("expiration_timestamp");
    }

    public Game getGame() {
        return new com.google.android.gms.games.b(this.zU, this.zW);
    }

    public Player getRecipient() {
        return new d(this.zU, this.zW, "recipient_");
    }

    public int getRecipientStatus() {
        return getInteger("recipient_status");
    }

    public int getRecipientStatus(String playerId) {
        for (int i = this.zW; i < this.zW + this.IN; i++) {
            int I = this.zU.I(i);
            if (this.zU.getString("recipient_external_player_id", i, I).equals(playerId)) {
                return this.zU.getInteger("recipient_status", i, I);
            }
        }
        return -1;
    }

    public String getRequestId() {
        return getString("external_request_id");
    }

    public Player getSender() {
        return new d(this.zU, this.zW, "sender_");
    }

    public int getType() {
        return getInteger("type");
    }

    public int hashCode() {
        return GameRequestEntity.a(this);
    }

    public boolean isConsumed() {
        return getRecipientStatus() == 1;
    }

    public String toString() {
        return GameRequestEntity.c(this);
    }

    public void writeToParcel(Parcel dest, int flags) {
        ((GameRequestEntity) freeze()).writeToParcel(dest, flags);
    }
}
