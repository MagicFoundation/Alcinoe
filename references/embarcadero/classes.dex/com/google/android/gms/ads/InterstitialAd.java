package com.google.android.gms.ads;

import android.content.Context;
import com.google.android.gms.internal.al;

public final class InterstitialAd {
    private final al kE;

    public InterstitialAd(Context context) {
        this.kE = new al(context);
    }

    public AdListener getAdListener() {
        return this.kE.getAdListener();
    }

    public String getAdUnitId() {
        return this.kE.getAdUnitId();
    }

    public boolean isLoaded() {
        return this.kE.isLoaded();
    }

    public void loadAd(AdRequest adRequest) {
        this.kE.a(adRequest.N());
    }

    public void setAdListener(AdListener adListener) {
        this.kE.setAdListener(adListener);
    }

    public void setAdUnitId(String adUnitId) {
        this.kE.setAdUnitId(adUnitId);
    }

    public void show() {
        this.kE.show();
    }
}
