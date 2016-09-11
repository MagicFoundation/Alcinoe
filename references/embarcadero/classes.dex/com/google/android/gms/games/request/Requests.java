package com.google.android.gms.games.request;

import android.content.Intent;
import android.graphics.Bitmap;
import android.os.Bundle;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Releasable;
import com.google.android.gms.common.api.Result;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public interface Requests {
    public static final String EXTRA_REQUESTS = "requests";
    public static final int REQUEST_DEFAULT_LIFETIME_DAYS = -1;
    public static final int REQUEST_DIRECTION_INBOUND = 0;
    public static final int REQUEST_DIRECTION_OUTBOUND = 1;
    public static final int REQUEST_UPDATE_OUTCOME_FAIL = 1;
    public static final int REQUEST_UPDATE_OUTCOME_RETRY = 2;
    public static final int REQUEST_UPDATE_OUTCOME_SUCCESS = 0;
    public static final int REQUEST_UPDATE_TYPE_ACCEPT = 0;
    public static final int REQUEST_UPDATE_TYPE_DISMISS = 1;
    public static final int SORT_ORDER_EXPIRING_SOON_FIRST = 0;
    public static final int SORT_ORDER_SOCIAL_AGGREGATION = 1;

    public interface LoadRequestsResult extends Releasable, Result {
        GameRequestBuffer getRequests(int i);
    }

    public interface UpdateRequestsResult extends Releasable, Result {
        Set<String> getRequestIds();

        int getRequestOutcome(String str);
    }

    PendingResult<UpdateRequestsResult> acceptRequest(GoogleApiClient googleApiClient, String str);

    PendingResult<UpdateRequestsResult> acceptRequests(GoogleApiClient googleApiClient, List<String> list);

    PendingResult<UpdateRequestsResult> dismissRequest(GoogleApiClient googleApiClient, String str);

    PendingResult<UpdateRequestsResult> dismissRequests(GoogleApiClient googleApiClient, List<String> list);

    ArrayList<GameRequest> getGameRequestsFromBundle(Bundle bundle);

    ArrayList<GameRequest> getGameRequestsFromInboxResponse(Intent intent);

    Intent getInboxIntent(GoogleApiClient googleApiClient);

    int getMaxLifetimeDays(GoogleApiClient googleApiClient);

    int getMaxPayloadSize(GoogleApiClient googleApiClient);

    Intent getSendIntent(GoogleApiClient googleApiClient, int i, byte[] bArr, int i2, Bitmap bitmap, String str);

    PendingResult<LoadRequestsResult> loadRequests(GoogleApiClient googleApiClient, int i, int i2, int i3);

    void registerRequestListener(GoogleApiClient googleApiClient, OnRequestReceivedListener onRequestReceivedListener);

    void unregisterRequestListener(GoogleApiClient googleApiClient);
}
