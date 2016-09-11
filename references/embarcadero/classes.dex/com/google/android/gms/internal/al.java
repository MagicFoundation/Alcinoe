package com.google.android.gms.internal;

import android.content.Context;
import android.os.RemoteException;
import com.google.android.gms.ads.AdListener;
import com.google.android.gms.ads.doubleclick.AppEventListener;

public final class al {
    private final be lF;
    private ag lG;
    private AdListener lc;
    private AppEventListener lq;
    private String ls;
    private final Context mContext;

    public al(Context context) {
        this.lF = new be();
        this.mContext = context;
    }

    private void k(String str) throws RemoteException {
        if (this.ls == null) {
            l(str);
        }
        this.lG = y.a(this.mContext, new ab(), this.ls, this.lF);
        if (this.lc != null) {
            this.lG.a(new x(this.lc));
        }
        if (this.lq != null) {
            this.lG.a(new ad(this.lq));
        }
    }

    private void l(String str) {
        if (this.lG == null) {
            throw new IllegalStateException("The ad unit ID must be set on InterstitialAd before " + str + " is called.");
        }
    }

    public void a(aj ajVar) {
        try {
            if (this.lG == null) {
                k("loadAd");
            }
            if (this.lG.a(new z(this.mContext, ajVar))) {
                this.lF.c(ajVar.ak());
            }
        } catch (Throwable e) {
            da.b("Failed to load ad.", e);
        }
    }

    public AdListener getAdListener() {
        return this.lc;
    }

    public String getAdUnitId() {
        return this.ls;
    }

    public AppEventListener getAppEventListener() {
        return this.lq;
    }

    public boolean isLoaded() {
        boolean z = false;
        try {
            if (this.lG != null) {
                z = this.lG.isReady();
            }
        } catch (Throwable e) {
            da.b("Failed to check if ad is ready.", e);
        }
        return z;
    }

    public void setAdListener(AdListener adListener) {
        try {
            this.lc = adListener;
            if (this.lG != null) {
                this.lG.a(adListener != null ? new x(adListener) : null);
            }
        } catch (Throwable e) {
            da.b("Failed to set the AdListener.", e);
        }
    }

    public void setAdUnitId(String adUnitId) {
        if (this.ls != null) {
            throw new IllegalStateException("The ad unit ID can only be set once on InterstitialAd.");
        }
        this.ls = adUnitId;
    }

    public void setAppEventListener(AppEventListener appEventListener) {
        try {
            this.lq = appEventListener;
            if (this.lG != null) {
                this.lG.a(appEventListener != null ? new ad(appEventListener) : null);
            }
        } catch (Throwable e) {
            da.b("Failed to set the AppEventListener.", e);
        }
    }

    public void show() {
        try {
            l("show");
            this.lG.showInterstitial();
        } catch (Throwable e) {
            da.b("Failed to show interstitial.", e);
        }
    }
}
