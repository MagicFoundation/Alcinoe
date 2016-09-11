package com.google.android.gms.ads.doubleclick;

import android.content.Context;
import com.google.android.gms.ads.AdListener;
import com.google.android.gms.internal.al;

public final class PublisherInterstitialAd {
    private final al kE;

    public PublisherInterstitialAd(Context context) {
        this.kE = new al(context);
    }

    public AdListener getAdListener() {
        return this.kE.getAdListener();
    }

    public String getAdUnitId() {
        return this.kE.getAdUnitId();
    }

    public AppEventListener getAppEventListener() {
        return this.kE.getAppEventListener();
    }

    public boolean isLoaded() {
        return this.kE.isLoaded();
    }

    public void loadAd(PublisherAdRequest publisherAdRequest) {
        this.kE.a(publisherAdRequest.N());
    }

    public void setAdListener(AdListener adListener) {
        this.kE.setAdListener(adListener);
    }

    public void setAdUnitId(String adUnitId) {
        this.kE.setAdUnitId(adUnitId);
    }

    public void setAppEventListener(AppEventListener appEventListener) {
        this.kE.setAppEventListener(appEventListener);
    }

    public void show() {
        this.kE.show();
    }
}
