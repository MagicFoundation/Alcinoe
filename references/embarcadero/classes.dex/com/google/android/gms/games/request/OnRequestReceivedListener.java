package com.google.android.gms.games.request;

public interface OnRequestReceivedListener {
    void onRequestReceived(GameRequest gameRequest);

    void onRequestRemoved(String str);
}
