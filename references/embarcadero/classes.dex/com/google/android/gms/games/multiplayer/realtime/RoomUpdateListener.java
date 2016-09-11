package com.google.android.gms.games.multiplayer.realtime;

public interface RoomUpdateListener {
    void onJoinedRoom(int i, Room room);

    void onLeftRoom(int i, String str);

    void onRoomConnected(int i, Room room);

    void onRoomCreated(int i, Room room);
}
