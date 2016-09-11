package com.embarcadero.firemonkey.advertising;

import com.google.android.gms.ads.AdListener;

public class AdListenerAdapter extends AdListener {
    private final IAdListener mListener;

    public AdListenerAdapter(IAdListener listener) {
        this.mListener = listener;
    }

    public void onAdLoaded() {
        this.mListener.onAdLoaded();
    }

    public void onAdFailedToLoad(int errorCode) {
        this.mListener.onAdFailedToLoad(errorCode);
    }

    public void onAdOpened() {
        this.mListener.onAdOpened();
    }

    public void onAdClosed() {
        this.mListener.onAdClosed();
    }

    public void onAdLeftApplication() {
        this.mListener.onAdLeftApplication();
    }
}
