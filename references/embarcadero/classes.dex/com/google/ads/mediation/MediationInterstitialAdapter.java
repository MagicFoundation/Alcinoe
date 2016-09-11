package com.google.ads.mediation;

import android.app.Activity;

@Deprecated
public interface MediationInterstitialAdapter<ADDITIONAL_PARAMETERS extends NetworkExtras, SERVER_PARAMETERS extends MediationServerParameters> extends MediationAdapter<ADDITIONAL_PARAMETERS, SERVER_PARAMETERS> {
    void requestInterstitialAd(MediationInterstitialListener mediationInterstitialListener, Activity activity, SERVER_PARAMETERS server_parameters, MediationAdRequest mediationAdRequest, ADDITIONAL_PARAMETERS additional_parameters);

    void showInterstitial();
}
