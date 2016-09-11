package com.google.android.gms.games;

import com.google.android.gms.common.api.GoogleApiClient;

public interface Notifications {
    public static final int NOTIFICATION_TYPES_ALL = 7;
    public static final int NOTIFICATION_TYPES_MULTIPLAYER = 3;
    public static final int NOTIFICATION_TYPE_INVITATION = 1;
    public static final int NOTIFICATION_TYPE_MATCH_UPDATE = 2;
    public static final int NOTIFICATION_TYPE_REQUEST = 4;

    void clear(GoogleApiClient googleApiClient, int i);

    void clearAll(GoogleApiClient googleApiClient);
}
