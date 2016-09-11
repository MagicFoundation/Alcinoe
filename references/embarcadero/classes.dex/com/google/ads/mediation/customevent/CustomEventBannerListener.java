package com.google.ads.mediation.customevent;

import android.view.View;

@Deprecated
public interface CustomEventBannerListener extends CustomEventListener {
    void onClick();

    void onReceivedAd(View view);
}
