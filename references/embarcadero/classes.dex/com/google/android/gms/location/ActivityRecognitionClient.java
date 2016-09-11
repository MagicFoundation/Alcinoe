package com.google.android.gms.location;

import android.app.PendingIntent;
import android.content.Context;
import com.google.android.gms.common.GooglePlayServicesClient;
import com.google.android.gms.common.GooglePlayServicesClient.ConnectionCallbacks;
import com.google.android.gms.common.GooglePlayServicesClient.OnConnectionFailedListener;
import com.google.android.gms.internal.hi;

public class ActivityRecognitionClient implements GooglePlayServicesClient {
    private final hi KO;

    public ActivityRecognitionClient(Context context, ConnectionCallbacks connectedListener, OnConnectionFailedListener connectionFailedListener) {
        this.KO = new hi(context, connectedListener, connectionFailedListener, "activity_recognition");
    }

    public void connect() {
        this.KO.connect();
    }

    public void disconnect() {
        this.KO.disconnect();
    }

    public boolean isConnected() {
        return this.KO.isConnected();
    }

    public boolean isConnecting() {
        return this.KO.isConnecting();
    }

    public boolean isConnectionCallbacksRegistered(ConnectionCallbacks listener) {
        return this.KO.isConnectionCallbacksRegistered(listener);
    }

    public boolean isConnectionFailedListenerRegistered(OnConnectionFailedListener listener) {
        return this.KO.isConnectionFailedListenerRegistered(listener);
    }

    public void registerConnectionCallbacks(ConnectionCallbacks listener) {
        this.KO.registerConnectionCallbacks(listener);
    }

    public void registerConnectionFailedListener(OnConnectionFailedListener listener) {
        this.KO.registerConnectionFailedListener(listener);
    }

    public void removeActivityUpdates(PendingIntent callbackIntent) {
        this.KO.removeActivityUpdates(callbackIntent);
    }

    public void requestActivityUpdates(long detectionIntervalMillis, PendingIntent callbackIntent) {
        this.KO.requestActivityUpdates(detectionIntervalMillis, callbackIntent);
    }

    public void unregisterConnectionCallbacks(ConnectionCallbacks listener) {
        this.KO.unregisterConnectionCallbacks(listener);
    }

    public void unregisterConnectionFailedListener(OnConnectionFailedListener listener) {
        this.KO.unregisterConnectionFailedListener(listener);
    }
}
