package com.google.android.gms.wallet;

import android.app.Activity;
import com.google.android.gms.common.GooglePlayServicesClient;
import com.google.android.gms.common.GooglePlayServicesClient.ConnectionCallbacks;
import com.google.android.gms.common.GooglePlayServicesClient.OnConnectionFailedListener;
import com.google.android.gms.internal.jg;

@Deprecated
public class WalletClient implements GooglePlayServicesClient {
    private final jg Zx;

    public WalletClient(Activity activity, int environment, String accountName, int theme, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener connectionFailedListener) {
        this.Zx = new jg(activity, connectionCallbacks, connectionFailedListener, environment, accountName, theme);
    }

    public WalletClient(Activity activity, int environment, String accountName, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener connectionFailedListener) {
        this(activity, environment, accountName, 0, connectionCallbacks, connectionFailedListener);
    }

    public void changeMaskedWallet(String googleTransactionId, String merchantTransactionId, int requestCode) {
        this.Zx.changeMaskedWallet(googleTransactionId, merchantTransactionId, requestCode);
    }

    public void checkForPreAuthorization(int requestCode) {
        this.Zx.checkForPreAuthorization(requestCode);
    }

    public void connect() {
        this.Zx.connect();
    }

    public void disconnect() {
        this.Zx.disconnect();
    }

    public boolean isConnected() {
        return this.Zx.isConnected();
    }

    public boolean isConnecting() {
        return this.Zx.isConnecting();
    }

    public boolean isConnectionCallbacksRegistered(ConnectionCallbacks listener) {
        return this.Zx.isConnectionCallbacksRegistered(listener);
    }

    public boolean isConnectionFailedListenerRegistered(OnConnectionFailedListener listener) {
        return this.Zx.isConnectionFailedListenerRegistered(listener);
    }

    public void loadFullWallet(FullWalletRequest request, int requestCode) {
        this.Zx.loadFullWallet(request, requestCode);
    }

    public void loadMaskedWallet(MaskedWalletRequest request, int requestCode) {
        this.Zx.loadMaskedWallet(request, requestCode);
    }

    public void notifyTransactionStatus(NotifyTransactionStatusRequest request) {
        this.Zx.notifyTransactionStatus(request);
    }

    public void registerConnectionCallbacks(ConnectionCallbacks listener) {
        this.Zx.registerConnectionCallbacks(listener);
    }

    public void registerConnectionFailedListener(OnConnectionFailedListener listener) {
        this.Zx.registerConnectionFailedListener(listener);
    }

    public void unregisterConnectionCallbacks(ConnectionCallbacks listener) {
        this.Zx.unregisterConnectionCallbacks(listener);
    }

    public void unregisterConnectionFailedListener(OnConnectionFailedListener listener) {
        this.Zx.unregisterConnectionFailedListener(listener);
    }
}
