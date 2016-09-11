package com.google.android.gms.internal;

import com.google.android.gms.ads.AdListener;
import com.google.android.gms.internal.af.a;

public final class x extends a {
    private final AdListener lc;

    public x(AdListener adListener) {
        this.lc = adListener;
    }

    public void onAdClosed() {
        this.lc.onAdClosed();
    }

    public void onAdFailedToLoad(int errorCode) {
        this.lc.onAdFailedToLoad(errorCode);
    }

    public void onAdLeftApplication() {
        this.lc.onAdLeftApplication();
    }

    public void onAdLoaded() {
        this.lc.onAdLoaded();
    }

    public void onAdOpened() {
        this.lc.onAdOpened();
    }
}
