package com.google.ads.mediation.customevent;

import android.app.Activity;
import com.google.ads.mediation.MediationAdRequest;

@Deprecated
public interface CustomEventInterstitial extends CustomEvent {
    void requestInterstitialAd(CustomEventInterstitialListener customEventInterstitialListener, Activity activity, String str, String str2, MediationAdRequest mediationAdRequest, Object obj);

    void showInterstitial();
}
