package com.google.android.gms.internal;

import com.google.ads.AdRequest.ErrorCode;
import com.google.ads.mediation.MediationBannerAdapter;
import com.google.ads.mediation.MediationBannerListener;
import com.google.ads.mediation.MediationInterstitialAdapter;
import com.google.ads.mediation.MediationInterstitialListener;
import com.google.ads.mediation.MediationServerParameters;
import com.google.ads.mediation.NetworkExtras;

public final class bj<NETWORK_EXTRAS extends NetworkExtras, SERVER_PARAMETERS extends MediationServerParameters> implements MediationBannerListener, MediationInterstitialListener {
    private final bh mT;

    /* renamed from: com.google.android.gms.internal.bj.10 */
    class AnonymousClass10 implements Runnable {
        final /* synthetic */ bj mU;
        final /* synthetic */ ErrorCode mV;

        AnonymousClass10(bj bjVar, ErrorCode errorCode) {
            this.mU = bjVar;
            this.mV = errorCode;
        }

        public void run() {
            try {
                this.mU.mT.onAdFailedToLoad(bk.a(this.mV));
            } catch (Throwable e) {
                da.b("Could not call onAdFailedToLoad.", e);
            }
        }
    }

    /* renamed from: com.google.android.gms.internal.bj.5 */
    class AnonymousClass5 implements Runnable {
        final /* synthetic */ bj mU;
        final /* synthetic */ ErrorCode mV;

        AnonymousClass5(bj bjVar, ErrorCode errorCode) {
            this.mU = bjVar;
            this.mV = errorCode;
        }

        public void run() {
            try {
                this.mU.mT.onAdFailedToLoad(bk.a(this.mV));
            } catch (Throwable e) {
                da.b("Could not call onAdFailedToLoad.", e);
            }
        }
    }

    public bj(bh bhVar) {
        this.mT = bhVar;
    }

    public void onClick(MediationBannerAdapter<?, ?> mediationBannerAdapter) {
        da.s("Adapter called onClick.");
        if (cz.aX()) {
            try {
                this.mT.O();
                return;
            } catch (Throwable e) {
                da.b("Could not call onAdClicked.", e);
                return;
            }
        }
        da.w("onClick must be called on the main UI thread.");
        cz.pT.post(new Runnable() {
            final /* synthetic */ bj mU;

            {
                this.mU = r1;
            }

            public void run() {
                try {
                    this.mU.mT.O();
                } catch (Throwable e) {
                    da.b("Could not call onAdClicked.", e);
                }
            }
        });
    }

    public void onDismissScreen(MediationBannerAdapter<?, ?> mediationBannerAdapter) {
        da.s("Adapter called onDismissScreen.");
        if (cz.aX()) {
            try {
                this.mT.onAdClosed();
                return;
            } catch (Throwable e) {
                da.b("Could not call onAdClosed.", e);
                return;
            }
        }
        da.w("onDismissScreen must be called on the main UI thread.");
        cz.pT.post(new Runnable() {
            final /* synthetic */ bj mU;

            {
                this.mU = r1;
            }

            public void run() {
                try {
                    this.mU.mT.onAdClosed();
                } catch (Throwable e) {
                    da.b("Could not call onAdClosed.", e);
                }
            }
        });
    }

    public void onDismissScreen(MediationInterstitialAdapter<?, ?> mediationInterstitialAdapter) {
        da.s("Adapter called onDismissScreen.");
        if (cz.aX()) {
            try {
                this.mT.onAdClosed();
                return;
            } catch (Throwable e) {
                da.b("Could not call onAdClosed.", e);
                return;
            }
        }
        da.w("onDismissScreen must be called on the main UI thread.");
        cz.pT.post(new Runnable() {
            final /* synthetic */ bj mU;

            {
                this.mU = r1;
            }

            public void run() {
                try {
                    this.mU.mT.onAdClosed();
                } catch (Throwable e) {
                    da.b("Could not call onAdClosed.", e);
                }
            }
        });
    }

    public void onFailedToReceiveAd(MediationBannerAdapter<?, ?> mediationBannerAdapter, ErrorCode errorCode) {
        da.s("Adapter called onFailedToReceiveAd with error. " + errorCode);
        if (cz.aX()) {
            try {
                this.mT.onAdFailedToLoad(bk.a(errorCode));
                return;
            } catch (Throwable e) {
                da.b("Could not call onAdFailedToLoad.", e);
                return;
            }
        }
        da.w("onFailedToReceiveAd must be called on the main UI thread.");
        cz.pT.post(new AnonymousClass5(this, errorCode));
    }

    public void onFailedToReceiveAd(MediationInterstitialAdapter<?, ?> mediationInterstitialAdapter, ErrorCode errorCode) {
        da.s("Adapter called onFailedToReceiveAd with error " + errorCode + ".");
        if (cz.aX()) {
            try {
                this.mT.onAdFailedToLoad(bk.a(errorCode));
                return;
            } catch (Throwable e) {
                da.b("Could not call onAdFailedToLoad.", e);
                return;
            }
        }
        da.w("onFailedToReceiveAd must be called on the main UI thread.");
        cz.pT.post(new AnonymousClass10(this, errorCode));
    }

    public void onLeaveApplication(MediationBannerAdapter<?, ?> mediationBannerAdapter) {
        da.s("Adapter called onLeaveApplication.");
        if (cz.aX()) {
            try {
                this.mT.onAdLeftApplication();
                return;
            } catch (Throwable e) {
                da.b("Could not call onAdLeftApplication.", e);
                return;
            }
        }
        da.w("onLeaveApplication must be called on the main UI thread.");
        cz.pT.post(new Runnable() {
            final /* synthetic */ bj mU;

            {
                this.mU = r1;
            }

            public void run() {
                try {
                    this.mU.mT.onAdLeftApplication();
                } catch (Throwable e) {
                    da.b("Could not call onAdLeftApplication.", e);
                }
            }
        });
    }

    public void onLeaveApplication(MediationInterstitialAdapter<?, ?> mediationInterstitialAdapter) {
        da.s("Adapter called onLeaveApplication.");
        if (cz.aX()) {
            try {
                this.mT.onAdLeftApplication();
                return;
            } catch (Throwable e) {
                da.b("Could not call onAdLeftApplication.", e);
                return;
            }
        }
        da.w("onLeaveApplication must be called on the main UI thread.");
        cz.pT.post(new Runnable() {
            final /* synthetic */ bj mU;

            {
                this.mU = r1;
            }

            public void run() {
                try {
                    this.mU.mT.onAdLeftApplication();
                } catch (Throwable e) {
                    da.b("Could not call onAdLeftApplication.", e);
                }
            }
        });
    }

    public void onPresentScreen(MediationBannerAdapter<?, ?> mediationBannerAdapter) {
        da.s("Adapter called onPresentScreen.");
        if (cz.aX()) {
            try {
                this.mT.onAdOpened();
                return;
            } catch (Throwable e) {
                da.b("Could not call onAdOpened.", e);
                return;
            }
        }
        da.w("onPresentScreen must be called on the main UI thread.");
        cz.pT.post(new Runnable() {
            final /* synthetic */ bj mU;

            {
                this.mU = r1;
            }

            public void run() {
                try {
                    this.mU.mT.onAdOpened();
                } catch (Throwable e) {
                    da.b("Could not call onAdOpened.", e);
                }
            }
        });
    }

    public void onPresentScreen(MediationInterstitialAdapter<?, ?> mediationInterstitialAdapter) {
        da.s("Adapter called onPresentScreen.");
        if (cz.aX()) {
            try {
                this.mT.onAdOpened();
                return;
            } catch (Throwable e) {
                da.b("Could not call onAdOpened.", e);
                return;
            }
        }
        da.w("onPresentScreen must be called on the main UI thread.");
        cz.pT.post(new Runnable() {
            final /* synthetic */ bj mU;

            {
                this.mU = r1;
            }

            public void run() {
                try {
                    this.mU.mT.onAdOpened();
                } catch (Throwable e) {
                    da.b("Could not call onAdOpened.", e);
                }
            }
        });
    }

    public void onReceivedAd(MediationBannerAdapter<?, ?> mediationBannerAdapter) {
        da.s("Adapter called onReceivedAd.");
        if (cz.aX()) {
            try {
                this.mT.onAdLoaded();
                return;
            } catch (Throwable e) {
                da.b("Could not call onAdLoaded.", e);
                return;
            }
        }
        da.w("onReceivedAd must be called on the main UI thread.");
        cz.pT.post(new Runnable() {
            final /* synthetic */ bj mU;

            {
                this.mU = r1;
            }

            public void run() {
                try {
                    this.mU.mT.onAdLoaded();
                } catch (Throwable e) {
                    da.b("Could not call onAdLoaded.", e);
                }
            }
        });
    }

    public void onReceivedAd(MediationInterstitialAdapter<?, ?> mediationInterstitialAdapter) {
        da.s("Adapter called onReceivedAd.");
        if (cz.aX()) {
            try {
                this.mT.onAdLoaded();
                return;
            } catch (Throwable e) {
                da.b("Could not call onAdLoaded.", e);
                return;
            }
        }
        da.w("onReceivedAd must be called on the main UI thread.");
        cz.pT.post(new Runnable() {
            final /* synthetic */ bj mU;

            {
                this.mU = r1;
            }

            public void run() {
                try {
                    this.mU.mT.onAdLoaded();
                } catch (Throwable e) {
                    da.b("Could not call onAdLoaded.", e);
                }
            }
        });
    }
}
