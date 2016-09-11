package com.google.android.gms.games.multiplayer;

import android.os.Parcel;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.games.Game;
import com.google.android.gms.internal.er;
import java.util.ArrayList;

public final class b extends com.google.android.gms.common.data.b implements Invitation {
    private final Game IM;
    private final ArrayList<Participant> Ju;
    private final d Jx;

    b(DataHolder dataHolder, int i, int i2) {
        super(dataHolder, i);
        this.IM = new com.google.android.gms.games.b(dataHolder, i);
        this.Ju = new ArrayList(i2);
        String string = getString("external_inviter_id");
        Object obj = null;
        for (int i3 = 0; i3 < i2; i3++) {
            d dVar = new d(this.zU, this.zW + i3);
            if (dVar.getParticipantId().equals(string)) {
                obj = dVar;
            }
            this.Ju.add(dVar);
        }
        this.Jx = (d) er.b(obj, (Object) "Must have a valid inviter!");
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object obj) {
        return InvitationEntity.a(this, obj);
    }

    public Invitation freeze() {
        return new InvitationEntity(this);
    }

    public int getAvailableAutoMatchSlots() {
        return !getBoolean("has_automatch_criteria") ? 0 : getInteger("automatch_max_players");
    }

    public long getCreationTimestamp() {
        return Math.max(getLong("creation_timestamp"), getLong("last_modified_timestamp"));
    }

    public Game getGame() {
        return this.IM;
    }

    public String getInvitationId() {
        return getString("external_invitation_id");
    }

    public int getInvitationType() {
        return getInteger("type");
    }

    public Participant getInviter() {
        return this.Jx;
    }

    public ArrayList<Participant> getParticipants() {
        return this.Ju;
    }

    public int getVariant() {
        return getInteger("variant");
    }

    public int hashCode() {
        return InvitationEntity.a(this);
    }

    public String toString() {
        return InvitationEntity.b((Invitation) this);
    }

    public void writeToParcel(Parcel dest, int flags) {
        ((InvitationEntity) freeze()).writeToParcel(dest, flags);
    }
}
