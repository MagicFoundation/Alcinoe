package com.google.android.gms.internal;

import android.content.Context;
import android.os.RemoteException;
import android.util.AttributeSet;
import android.view.View;
import android.view.ViewGroup;
import com.google.android.gms.ads.AdListener;
import com.google.android.gms.ads.AdSize;
import com.google.android.gms.ads.doubleclick.AppEventListener;
import com.google.android.gms.dynamic.b;
import com.google.android.gms.dynamic.c;

public final class ak {
    private final be lF;
    private ag lG;
    private ViewGroup lH;
    private AdListener lc;
    private AppEventListener lq;
    private AdSize[] lr;
    private String ls;

    public ak(ViewGroup viewGroup) {
        this.lF = new be();
        this.lH = viewGroup;
    }

    public ak(ViewGroup viewGroup, AttributeSet attributeSet, boolean z) {
        this.lF = new be();
        this.lH = viewGroup;
        Context context = viewGroup.getContext();
        try {
            ae aeVar = new ae(context, attributeSet);
            this.lr = aeVar.c(z);
            this.ls = aeVar.getAdUnitId();
            if (viewGroup.isInEditMode()) {
                cz.a(viewGroup, new ab(context, this.lr[0]), "Ads by Google");
            }
        } catch (IllegalArgumentException e) {
            cz.a(viewGroup, new ab(context, AdSize.BANNER), e.getMessage(), e.getMessage());
        }
    }

    private void am() {
        try {
            b P = this.lG.P();
            if (P != null) {
                this.lH.addView((View) c.b(P));
            }
        } catch (Throwable e) {
            da.b("Failed to get an ad frame.", e);
        }
    }

    private void an() throws RemoteException {
        if ((this.lr == null || this.ls == null) && this.lG == null) {
            throw new IllegalStateException("The ad size and ad unit ID must be set before loadAd is called.");
        }
        Context context = this.lH.getContext();
        this.lG = y.a(context, new ab(context, this.lr), this.ls, this.lF);
        if (this.lc != null) {
            this.lG.a(new x(this.lc));
        }
        if (this.lq != null) {
            this.lG.a(new ad(this.lq));
        }
        am();
    }

    public void a(aj ajVar) {
        try {
            if (this.lG == null) {
                an();
            }
            if (this.lG.a(new z(this.lH.getContext(), ajVar))) {
                this.lF.c(ajVar.ak());
            }
        } catch (Throwable e) {
            da.b("Failed to load ad.", e);
        }
    }

    public void a(AdSize... adSizeArr) {
        this.lr = adSizeArr;
        try {
            if (this.lG != null) {
                this.lG.a(new ab(this.lH.getContext(), this.lr));
            }
        } catch (Throwable e) {
            da.b("Failed to set the ad size.", e);
        }
        this.lH.requestLayout();
    }

    public void destroy() {
        try {
            if (this.lG != null) {
                this.lG.destroy();
            }
        } catch (Throwable e) {
            da.b("Failed to destroy AdView.", e);
        }
    }

    public AdListener getAdListener() {
        return this.lc;
    }

    public AdSize getAdSize() {
        try {
            if (this.lG != null) {
                return this.lG.Q().ai();
            }
        } catch (Throwable e) {
            da.b("Failed to get the current AdSize.", e);
        }
        return this.lr != null ? this.lr[0] : null;
    }

    public AdSize[] getAdSizes() {
        return this.lr;
    }

    public String getAdUnitId() {
        return this.ls;
    }

    public AppEventListener getAppEventListener() {
        return this.lq;
    }

    public void pause() {
        try {
            if (this.lG != null) {
                this.lG.pause();
            }
        } catch (Throwable e) {
            da.b("Failed to call pause.", e);
        }
    }

    public void recordManualImpression() {
        try {
            this.lG.Z();
        } catch (Throwable e) {
            da.b("Failed to record impression.", e);
        }
    }

    public void resume() {
        try {
            if (this.lG != null) {
                this.lG.resume();
            }
        } catch (Throwable e) {
            da.b("Failed to call resume.", e);
        }
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

    public void setAdSizes(AdSize... adSizes) {
        if (this.lr != null) {
            throw new IllegalStateException("The ad size can only be set once on AdView.");
        }
        a(adSizes);
    }

    public void setAdUnitId(String adUnitId) {
        if (this.ls != null) {
            throw new IllegalStateException("The ad unit ID can only be set once on AdView.");
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
}
