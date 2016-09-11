package com.google.android.gms.games.multiplayer.realtime;

import android.database.CharArrayBuffer;
import android.os.Bundle;
import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.games.Player;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.games.multiplayer.ParticipantEntity;
import com.google.android.gms.internal.eg;
import com.google.android.gms.internal.ep;
import com.google.android.gms.internal.fm;
import com.google.android.gms.internal.fy;
import java.util.ArrayList;

public final class RoomEntity extends fy implements Room {
    public static final Creator<RoomEntity> CREATOR;
    private final String FH;
    private final String GU;
    private final Bundle JO;
    private final String JS;
    private final int JT;
    private final int JU;
    private final long Jr;
    private final ArrayList<ParticipantEntity> Ju;
    private final int Jv;
    private final int wj;

    static final class a extends b {
        a() {
        }

        public RoomEntity aq(Parcel parcel) {
            if (fy.c(eg.dY()) || eg.ae(RoomEntity.class.getCanonicalName())) {
                return super.aq(parcel);
            }
            String readString = parcel.readString();
            String readString2 = parcel.readString();
            long readLong = parcel.readLong();
            int readInt = parcel.readInt();
            String readString3 = parcel.readString();
            int readInt2 = parcel.readInt();
            Bundle readBundle = parcel.readBundle();
            int readInt3 = parcel.readInt();
            ArrayList arrayList = new ArrayList(readInt3);
            for (int i = 0; i < readInt3; i++) {
                arrayList.add(ParticipantEntity.CREATOR.createFromParcel(parcel));
            }
            return new RoomEntity(2, readString, readString2, readLong, readInt, readString3, readInt2, readBundle, arrayList, -1);
        }

        public /* synthetic */ Object createFromParcel(Parcel x0) {
            return aq(x0);
        }
    }

    static {
        CREATOR = new a();
    }

    RoomEntity(int versionCode, String roomId, String creatorId, long creationTimestamp, int roomStatus, String description, int variant, Bundle autoMatchCriteria, ArrayList<ParticipantEntity> participants, int autoMatchWaitEstimateSeconds) {
        this.wj = versionCode;
        this.GU = roomId;
        this.JS = creatorId;
        this.Jr = creationTimestamp;
        this.JT = roomStatus;
        this.FH = description;
        this.Jv = variant;
        this.JO = autoMatchCriteria;
        this.Ju = participants;
        this.JU = autoMatchWaitEstimateSeconds;
    }

    public RoomEntity(Room room) {
        this.wj = 2;
        this.GU = room.getRoomId();
        this.JS = room.getCreatorId();
        this.Jr = room.getCreationTimestamp();
        this.JT = room.getStatus();
        this.FH = room.getDescription();
        this.Jv = room.getVariant();
        this.JO = room.getAutoMatchCriteria();
        ArrayList participants = room.getParticipants();
        int size = participants.size();
        this.Ju = new ArrayList(size);
        for (int i = 0; i < size; i++) {
            this.Ju.add((ParticipantEntity) ((Participant) participants.get(i)).freeze());
        }
        this.JU = room.getAutoMatchWaitEstimateSeconds();
    }

    static int a(Room room) {
        return ep.hashCode(room.getRoomId(), room.getCreatorId(), Long.valueOf(room.getCreationTimestamp()), Integer.valueOf(room.getStatus()), room.getDescription(), Integer.valueOf(room.getVariant()), room.getAutoMatchCriteria(), room.getParticipants(), Integer.valueOf(room.getAutoMatchWaitEstimateSeconds()));
    }

    static int a(Room room, String str) {
        ArrayList participants = room.getParticipants();
        int size = participants.size();
        for (int i = 0; i < size; i++) {
            Participant participant = (Participant) participants.get(i);
            if (participant.getParticipantId().equals(str)) {
                return participant.getStatus();
            }
        }
        throw new IllegalStateException("Participant " + str + " is not in room " + room.getRoomId());
    }

    static boolean a(Room room, Object obj) {
        if (!(obj instanceof Room)) {
            return false;
        }
        if (room == obj) {
            return true;
        }
        Room room2 = (Room) obj;
        return ep.equal(room2.getRoomId(), room.getRoomId()) && ep.equal(room2.getCreatorId(), room.getCreatorId()) && ep.equal(Long.valueOf(room2.getCreationTimestamp()), Long.valueOf(room.getCreationTimestamp())) && ep.equal(Integer.valueOf(room2.getStatus()), Integer.valueOf(room.getStatus())) && ep.equal(room2.getDescription(), room.getDescription()) && ep.equal(Integer.valueOf(room2.getVariant()), Integer.valueOf(room.getVariant())) && ep.equal(room2.getAutoMatchCriteria(), room.getAutoMatchCriteria()) && ep.equal(room2.getParticipants(), room.getParticipants()) && ep.equal(Integer.valueOf(room2.getAutoMatchWaitEstimateSeconds()), Integer.valueOf(room.getAutoMatchWaitEstimateSeconds()));
    }

    static String b(Room room) {
        return ep.e(room).a("RoomId", room.getRoomId()).a("CreatorId", room.getCreatorId()).a("CreationTimestamp", Long.valueOf(room.getCreationTimestamp())).a("RoomStatus", Integer.valueOf(room.getStatus())).a("Description", room.getDescription()).a("Variant", Integer.valueOf(room.getVariant())).a("AutoMatchCriteria", room.getAutoMatchCriteria()).a("Participants", room.getParticipants()).a("AutoMatchWaitEstimateSeconds", Integer.valueOf(room.getAutoMatchWaitEstimateSeconds())).toString();
    }

    static String b(Room room, String str) {
        ArrayList participants = room.getParticipants();
        int size = participants.size();
        for (int i = 0; i < size; i++) {
            Participant participant = (Participant) participants.get(i);
            Player player = participant.getPlayer();
            if (player != null && player.getPlayerId().equals(str)) {
                return participant.getParticipantId();
            }
        }
        return null;
    }

    static Participant c(Room room, String str) {
        ArrayList participants = room.getParticipants();
        int size = participants.size();
        for (int i = 0; i < size; i++) {
            Participant participant = (Participant) participants.get(i);
            if (participant.getParticipantId().equals(str)) {
                return participant;
            }
        }
        throw new IllegalStateException("Participant " + str + " is not in match " + room.getRoomId());
    }

    static ArrayList<String> c(Room room) {
        ArrayList participants = room.getParticipants();
        int size = participants.size();
        ArrayList<String> arrayList = new ArrayList(size);
        for (int i = 0; i < size; i++) {
            arrayList.add(((Participant) participants.get(i)).getParticipantId());
        }
        return arrayList;
    }

    public int describeContents() {
        return 0;
    }

    public boolean equals(Object obj) {
        return a((Room) this, obj);
    }

    public Room freeze() {
        return this;
    }

    public Bundle getAutoMatchCriteria() {
        return this.JO;
    }

    public int getAutoMatchWaitEstimateSeconds() {
        return this.JU;
    }

    public long getCreationTimestamp() {
        return this.Jr;
    }

    public String getCreatorId() {
        return this.JS;
    }

    public String getDescription() {
        return this.FH;
    }

    public void getDescription(CharArrayBuffer dataOut) {
        fm.b(this.FH, dataOut);
    }

    public Participant getParticipant(String participantId) {
        return c(this, participantId);
    }

    public String getParticipantId(String playerId) {
        return b(this, playerId);
    }

    public ArrayList<String> getParticipantIds() {
        return c(this);
    }

    public int getParticipantStatus(String participantId) {
        return a((Room) this, participantId);
    }

    public ArrayList<Participant> getParticipants() {
        return new ArrayList(this.Ju);
    }

    public String getRoomId() {
        return this.GU;
    }

    public int getStatus() {
        return this.JT;
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
        return b((Room) this);
    }

    public void writeToParcel(Parcel dest, int flags) {
        if (dZ()) {
            dest.writeString(this.GU);
            dest.writeString(this.JS);
            dest.writeLong(this.Jr);
            dest.writeInt(this.JT);
            dest.writeString(this.FH);
            dest.writeInt(this.Jv);
            dest.writeBundle(this.JO);
            int size = this.Ju.size();
            dest.writeInt(size);
            for (int i = 0; i < size; i++) {
                ((ParticipantEntity) this.Ju.get(i)).writeToParcel(dest, flags);
            }
            return;
        }
        b.a(this, dest, flags);
    }
}
