package com.embarcadero.firemonkey.advertising;

public interface IAdListener {
    void onAdClosed();

    void onAdFailedToLoad(int i);

    void onAdLeftApplication();

    void onAdLoaded();

    void onAdOpened();
}
