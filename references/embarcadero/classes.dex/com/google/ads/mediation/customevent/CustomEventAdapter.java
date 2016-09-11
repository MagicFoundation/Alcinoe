package com.google.ads.mediation.customevent;

import android.app.Activity;
import android.view.View;
import com.google.ads.AdRequest.ErrorCode;
import com.google.ads.AdSize;
import com.google.ads.mediation.MediationAdRequest;
import com.google.ads.mediation.MediationBannerAdapter;
import com.google.ads.mediation.MediationBannerListener;
import com.google.ads.mediation.MediationInterstitialAdapter;
import com.google.ads.mediation.MediationInterstitialListener;
import com.google.android.gms.ads.mediation.customevent.CustomEventExtras;
import com.google.android.gms.internal.da;

public final class CustomEventAdapter implements MediationBannerAdapter<CustomEventExtras, CustomEventServerParameters>, MediationInterstitialAdapter<CustomEventExtras, CustomEventServerParameters> {
    private View m;
    private CustomEventBanner n;
    private CustomEventInterstitial o;

    private static final class a implements CustomEventBannerListener {
        private final MediationBannerListener k;
        private final CustomEventAdapter p;

        public a(CustomEventAdapter customEventAdapter, MediationBannerListener mediationBannerListener) {
            this.p = customEventAdapter;
            this.k = mediationBannerListener;
        }

        public void onClick() {
            da.s("Custom event adapter called onFailedToReceiveAd.");
            this.k.onClick(this.p);
        }

        public void onDismissScreen() {
            da.s("Custom event adapter called onFailedToReceiveAd.");
            this.k.onDismissScreen(this.p);
        }

        public void onFailedToReceiveAd() {
            da.s("Custom event adapter called onFailedToReceiveAd.");
            this.k.onFailedToReceiveAd(this.p, ErrorCode.NO_FILL);
        }

        public void onLeaveApplication() {
            da.s("Custom event adapter called onFailedToReceiveAd.");
            this.k.onLeaveApplication(this.p);
        }

        public void onPresentScreen() {
            da.s("Custom event adapter called onFailedToReceiveAd.");
            this.k.onPresentScreen(this.p);
        }

        public void onReceivedAd(View view) {
            da.s("Custom event adapter called onReceivedAd.");
            this.p.a(view);
            this.k.onReceivedAd(this.p);
        }
    }

    private class b implements CustomEventInterstitialListener {
        private final MediationInterstitialListener l;
        private final CustomEventAdapter p;
        final /* synthetic */ CustomEventAdapter q;

        public b(CustomEventAdapter customEventAdapter, CustomEventAdapter customEventAdapter2, MediationInterstitialListener mediationInterstitialListener) {
            this.q = customEventAdapter;
            this.p = customEventAdapter2;
            this.l = mediationInterstitialListener;
        }

        public void onDismissScreen() {
            da.s("Custom event adapter called onDismissScreen.");
            this.l.onDismissScreen(this.p);
        }

        public void onFailedToReceiveAd() {
            da.s("Custom event adapter called onFailedToReceiveAd.");
            this.l.onFailedToReceiveAd(this.p, ErrorCode.NO_FILL);
        }

        public void onLeaveApplication() {
            da.s("Custom event adapter called onLeaveApplication.");
            this.l.onLeaveApplication(this.p);
        }

        public void onPresentScreen() {
            da.s("Custom event adapter called onPresentScreen.");
            this.l.onPresentScreen(this.p);
        }

        public void onReceivedAd() {
            da.s("Custom event adapter called onReceivedAd.");
            this.l.onReceivedAd(this.q);
        }
    }

    private static <T> T a(String str) {
        try {
            return Class.forName(str).newInstance();
        } catch (Throwable th) {
            da.w("Could not instantiate custom event adapter: " + str + ". " + th.getMessage());
            return null;
        }
    }

    private void a(View view) {
        this.m = view;
    }

    public void destroy() {
        if (this.n != null) {
            this.n.destroy();
        }
        if (this.o != null) {
            this.o.destroy();
        }
    }

    public Class<CustomEventExtras> getAdditionalParametersType() {
        return CustomEventExtras.class;
    }

    public View getBannerView() {
        return this.m;
    }

    public Class<CustomEventServerParameters> getServerParametersType() {
        return CustomEventServerParameters.class;
    }

    public void requestBannerAd(MediationBannerListener listener, Activity activity, CustomEventServerParameters serverParameters, AdSize adSize, MediationAdRequest mediationAdRequest, CustomEventExtras customEventExtras) {
        this.n = (CustomEventBanner) a(serverParameters.className);
        if (this.n == null) {
            listener.onFailedToReceiveAd(this, ErrorCode.INTERNAL_ERROR);
        } else {
            this.n.requestBannerAd(new a(this, listener), activity, serverParameters.label, serverParameters.parameter, adSize, mediationAdRequest, customEventExtras == null ? null : customEventExtras.getExtra(serverParameters.label));
        }
    }

    public void requestInterstitialAd(MediationInterstitialListener listener, Activity activity, CustomEventServerParameters serverParameters, MediationAdRequest mediationAdRequest, CustomEventExtras customEventExtras) {
        this.o = (CustomEventInterstitial) a(serverParameters.className);
        if (this.o == null) {
            listener.onFailedToReceiveAd(this, ErrorCode.INTERNAL_ERROR);
        } else {
            this.o.requestInterstitialAd(new b(this, this, listener), activity, serverParameters.label, serverParameters.parameter, mediationAdRequest, customEventExtras == null ? null : customEventExtras.getExtra(serverParameters.label));
        }
    }

    public void showInterstitial() {
        this.o.showInterstitial();
    }
}
