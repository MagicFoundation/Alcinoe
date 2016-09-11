package com.google.android.gms.games.multiplayer;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.games.Game;
import com.google.android.gms.games.GameEntity;
import com.google.android.gms.internal.eg;
import com.google.android.gms.internal.ep;
import com.google.android.gms.internal.er;
import com.google.android.gms.internal.fy;
import java.util.ArrayList;

public final class InvitationEntity extends fy implements Invitation {
    public static final Creator<InvitationEntity> CREATOR;
    private final String GP;
    private final GameEntity Jq;
    private final long Jr;
    private final int Js;
    private final ParticipantEntity Jt;
    private final ArrayList<ParticipantEntity> Ju;
    private final int Jv;
    private final int Jw;
    private final int wj;

    static final class a extends a {
        a() {
        }

        public InvitationEntity an(Parcel parcel) {
            if (fy.c(eg.dY()) || eg.ae(InvitationEntity.class.getCanonicalName())) {
                return super.an(parcel);
            }
            GameEntity gameEntity = (GameEntity) GameEntity.CREATOR.createFromParcel(parcel);
            String readString = parcel.readString();
            long readLong = parcel.readLong();
            int readInt = parcel.readInt();
            ParticipantEntity participantEntity = (ParticipantEntity) ParticipantEntity.CREATOR.createFromParcel(parcel);
            int readInt2 = parcel.readInt();
            ArrayList arrayList = new ArrayList(readInt2);
            for (int i = 0; i < readInt2; i++) {
                arrayList.add(ParticipantEntity.CREATOR.createFromParcel(parcel));
            }
            return new InvitationEntity(2, gameEntity, readString, readLong, readInt, participantEntity, arrayList, -1, 0);
        }

        public /* synthetic */ Object createFromParcel(Parcel x0) {
            return an(x0);
        }
    }

    static {
        CREATOR = new a();
    }

    InvitationEntity(int versionCode, GameEntity game, String invitationId, long creationTimestamp, int invitationType, ParticipantEntity inviter, ArrayList<ParticipantEntity> participants, int variant, int availableAutoMatchSlots) {
        this.wj = versionCode;
        this.Jq = game;
        this.GP = invitationId;
        this.Jr = creationTimestamp;
        this.Js = invitationType;
        this.Jt = inviter;
        this.Ju = participants;
        this.Jv = variant;
        this.Jw = availableAutoMatchSlots;
    }

    InvitationEntity(Invitation invitation) {
        this.wj = 2;
        this.Jq = new GameEntity(invitation.getGame());
        this.GP = invitation.getInvitationId();
        this.Jr = invitation.getCreationTimestamp();
        this.Js = invitation.getInvitationType();
        this.Jv = invitation.getVariant();
        this.Jw = invitation.getAvailableAutoMatchSlots();
        String participantId = invitation.getInviter().getParticipantId();
        Object obj = null;
        ArrayList participants = invitation.getParticipants();
        int size = participants.size();
        this.Ju = new ArrayList(size);
        for (int i = 0; i < size; i++) {
            Participant participant = (Participant) participants.get(i);
            if (participant.getParticipantId().equals(participantId)) {
                obj = participant;
            }
            this.Ju.add((ParticipantEntity) participant.freeze());
        }
        er.b(obj, (Object) "Must have a valid inviter!");
        this.Jt = (ParticipantEntity) obj.freeze();
    }

    static int a(Invitation invitation) {
        return ep.hashCode(invitation.getGame(), invitation.getInvitationId(), Long.valueOf(invitation.getCreationTimestamp()), Integer.valueOf(invitation.getInvitationType()), invitation.getInviter(), invitation.getParticipants(), Integer.valueOf(invitation.getVariant()), Integer.valueOf(invitation.getAvailableAutoMatchSlots()));
    }

    static boolean a(Invitation invitation, Object obj) {
        if (!(obj instanceof Invitation)) {
            return false;
        }
        if (invitation == obj) {
            return true;
        }
        Invitation invitation2 = (Invitation) obj;
        return ep.equal(invitation2.getGame(), invitation.getGame()) && ep.equal(invitation2.getInvitationId(), invitation.getInvitationId()) && ep.equal(Long.valueOf(invitation2.getCreationTimestamp()), Long.valueOf(invitation.getCreationTimestamp())) && ep.equal(Integer.valueOf(invitation2.getInvitationType()), Integer.valueOf(invitation.getInvitationType())) && ep.equal(invitation2.getInviter(), invitation.getInviter()) && ep.equal(invitation2.getParticipants(), invitation.getParticipants()) && ep.equal(Integer.valueOf(invitation2.getVariant()), Integer.valueOf(invitation.getVariant())) && ep.equal(Integer.valueOf(invitation2.getAvailableAutoMatchSlots()), Integer.valueOf(invitation.getAvailableAutoMatchSlots()));
    }

    static String b(Invitation invitation) {
        return ep.e(invitation).a("Game", invitation.getGame()).a("InvitationId", invitation.getInvitationId()).a("CreationTimestamp", Long.valueOf(invitation.getCreationTimestamp())).a("InvitationType", Integer.valueOf(invitation.getInvitationType())).a("Inviter", invitation.getInviter()).a("Participants", invitation.getParticipants()).a("Variant", Integer.valueOf(invitation.getVariant())).a("AvailableAutoMatchSlots", Integer.valueOf(invitation.getAvailableAutoMatchSlots())).toString();
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object obj) {
        return a(this, obj);
    }

    public Invitation freeze() {
        return this;
    }

    public int getAvailableAutoMatchSlots() {
        return this.Jw;
    }

    public long getCreationTimestamp() {
        return this.Jr;
    }

    public Game getGame() {
        return this.Jq;
    }

    public String getInvitationId() {
        return this.GP;
    }

    public int getInvitationType() {
        return this.Js;
    }

    public Participant getInviter() {
        return this.Jt;
    }

    public ArrayList<Participant> getParticipants() {
        return new ArrayList(this.Ju);
    }

    public int getVariant() {
        return this.Jv;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public int hashCode() {
        return a(this);
    }

    public boolean isDataValid() {
        return true;
    }

    public String toString() {
        return b((Invitation) this);
    }

    public void writeToParcel(Parcel dest, int flags) {
        if (dZ()) {
            this.Jq.writeToParcel(dest, flags);
            dest.writeString(this.GP);
            dest.writeLong(this.Jr);
            dest.writeInt(this.Js);
            this.Jt.writeToParcel(dest, flags);
            int size = this.Ju.size();
            dest.writeInt(size);
            for (int i = 0; i < size; i++) {
                ((ParticipantEntity) this.Ju.get(i)).writeToParcel(dest, flags);
            }
            return;
        }
        a.a(this, dest, flags);
    }
}
