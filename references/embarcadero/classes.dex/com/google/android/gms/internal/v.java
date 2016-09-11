package com.google.android.gms.internal;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.Bundle;
import android.os.RemoteException;
import android.util.DisplayMetrics;
import android.view.View;
import android.view.ViewGroup.LayoutParams;
import android.view.ViewParent;
import android.widget.ViewSwitcher;
import com.google.android.gms.dynamic.b;
import com.google.android.gms.dynamic.c;
import java.util.HashSet;

public final class v extends com.google.android.gms.internal.ag.a implements ap, az, br, bu, com.google.android.gms.internal.by.a, u {
    private final bf kH;
    private final a kI;
    private final w kJ;

    public static final class a {
        public final String adUnitId;
        public final ViewSwitcher kK;
        public final Context kL;
        public final l kM;
        public final db kN;
        public af kO;
        public ct kP;
        public ab kQ;
        public cn kR;
        public co kS;
        public ai kT;
        public cr kU;
        private HashSet<co> kV;

        public a(Context context, ab abVar, String str, db dbVar) {
            this.kU = null;
            this.kV = null;
            if (abVar.lo) {
                this.kK = null;
            } else {
                this.kK = new ViewSwitcher(context);
                this.kK.setMinimumWidth(abVar.widthPixels);
                this.kK.setMinimumHeight(abVar.heightPixels);
                this.kK.setVisibility(4);
            }
            this.kQ = abVar;
            this.adUnitId = str;
            this.kL = context;
            this.kM = new l(k.a(dbVar.pU, context));
            this.kN = dbVar;
        }

        public void a(HashSet<co> hashSet) {
            this.kV = hashSet;
        }

        public HashSet<co> ah() {
            return this.kV;
        }
    }

    public v(Context context, ab abVar, String str, bf bfVar, db dbVar) {
        this.kI = new a(context, abVar, str, dbVar);
        this.kH = bfVar;
        this.kJ = new w(this);
        da.u("Use AdRequest.Builder.addTestDevice(\"" + cz.l(context) + "\") to get test ads on this device.");
        cv.i(context);
    }

    private void a(int i) {
        da.w("Failed to load ad: " + i);
        if (this.kI.kO != null) {
            try {
                this.kI.kO.onAdFailedToLoad(i);
            } catch (Throwable e) {
                da.b("Could not call AdListener.onAdFailedToLoad().", e);
            }
        }
    }

    private void aa() {
        da.u("Ad closing.");
        if (this.kI.kO != null) {
            try {
                this.kI.kO.onAdClosed();
            } catch (Throwable e) {
                da.b("Could not call AdListener.onAdClosed().", e);
            }
        }
    }

    private void ab() {
        da.u("Ad leaving application.");
        if (this.kI.kO != null) {
            try {
                this.kI.kO.onAdLeftApplication();
            } catch (Throwable e) {
                da.b("Could not call AdListener.onAdLeftApplication().", e);
            }
        }
    }

    private void ac() {
        da.u("Ad opening.");
        if (this.kI.kO != null) {
            try {
                this.kI.kO.onAdOpened();
            } catch (Throwable e) {
                da.b("Could not call AdListener.onAdOpened().", e);
            }
        }
    }

    private void ad() {
        da.u("Ad finished loading.");
        if (this.kI.kO != null) {
            try {
                this.kI.kO.onAdLoaded();
            } catch (Throwable e) {
                da.b("Could not call AdListener.onAdLoaded().", e);
            }
        }
    }

    private boolean ae() {
        boolean z = true;
        if (!cv.a(this.kI.kL.getPackageManager(), this.kI.kL.getPackageName(), "android.permission.INTERNET")) {
            if (!this.kI.kQ.lo) {
                cz.a(this.kI.kK, this.kI.kQ, "Missing internet permission in AndroidManifest.xml.", "Missing internet permission in AndroidManifest.xml. You must have the following declaration: <uses-permission android:name=\"android.permission.INTERNET\" />");
            }
            z = false;
        }
        if (!cv.h(this.kI.kL)) {
            if (!this.kI.kQ.lo) {
                cz.a(this.kI.kK, this.kI.kQ, "Missing AdActivity with android:configChanges in AndroidManifest.xml.", "Missing AdActivity with android:configChanges in AndroidManifest.xml. You must have the following declaration within the <application> element: <activity android:name=\"com.google.android.gms.ads.AdActivity\" android:configChanges=\"keyboard|keyboardHidden|orientation|screenLayout|uiMode|screenSize|smallestScreenSize\" />");
            }
            z = false;
        }
        if (!(z || this.kI.kQ.lo)) {
            this.kI.kK.setVisibility(0);
        }
        return z;
    }

    private void af() {
        if (this.kI.kR == null) {
            da.w("Ad state was null when trying to ping click URLs.");
            return;
        }
        da.s("Pinging click URLs.");
        this.kI.kS.aK();
        if (this.kI.kR.mt != null) {
            cv.a(this.kI.kL, this.kI.kN.pU, this.kI.kR.mt);
        }
        if (this.kI.kR.pf != null && this.kI.kR.pf.mt != null) {
            bd.a(this.kI.kL, this.kI.kN.pU, this.kI.kR, this.kI.adUnitId, false, this.kI.kR.pf.mt);
        }
    }

    private void ag() {
        if (this.kI.kR != null) {
            this.kI.kR.nu.destroy();
            this.kI.kR = null;
        }
    }

    private void b(View view) {
        this.kI.kK.addView(view, new LayoutParams(-2, -2));
    }

    private void b(boolean z) {
        if (this.kI.kR == null) {
            da.w("Ad state was null when trying to ping impression URLs.");
            return;
        }
        da.s("Pinging Impression URLs.");
        this.kI.kS.aJ();
        if (this.kI.kR.mu != null) {
            cv.a(this.kI.kL, this.kI.kN.pU, this.kI.kR.mu);
        }
        if (!(this.kI.kR.pf == null || this.kI.kR.pf.mu == null)) {
            bd.a(this.kI.kL, this.kI.kN.pU, this.kI.kR, this.kI.adUnitId, z, this.kI.kR.pf.mu);
        }
        if (this.kI.kR.mM != null && this.kI.kR.mM.mp != null) {
            bd.a(this.kI.kL, this.kI.kN.pU, this.kI.kR, this.kI.adUnitId, z, this.kI.kR.mM.mp);
        }
    }

    private boolean b(cn cnVar) {
        View view;
        if (cnVar.ok) {
            try {
                view = (View) c.b(cnVar.mN.getView());
                View nextView = this.kI.kK.getNextView();
                if (nextView != null) {
                    this.kI.kK.removeView(nextView);
                }
                try {
                    b(view);
                } catch (Throwable th) {
                    da.b("Could not add mediation view to view hierarchy.", th);
                    return false;
                }
            } catch (Throwable th2) {
                da.b("Could not get View from mediation adapter.", th2);
                return false;
            }
        } else if (cnVar.pg != null) {
            cnVar.nu.a(cnVar.pg);
            this.kI.kK.removeAllViews();
            this.kI.kK.setMinimumWidth(cnVar.pg.widthPixels);
            this.kI.kK.setMinimumHeight(cnVar.pg.heightPixels);
            b(cnVar.nu);
        }
        if (this.kI.kK.getChildCount() > 1) {
            this.kI.kK.showNext();
        }
        if (this.kI.kR != null) {
            view = this.kI.kK.getNextView();
            if (view instanceof dd) {
                ((dd) view).a(this.kI.kL, this.kI.kQ);
            } else if (view != null) {
                this.kI.kK.removeView(view);
            }
            if (this.kI.kR.mN != null) {
                try {
                    this.kI.kR.mN.destroy();
                } catch (RemoteException e) {
                    da.w("Could not destroy previous mediation adapter.");
                }
            }
        }
        this.kI.kK.setVisibility(0);
        return true;
    }

    private com.google.android.gms.internal.cd.a c(z zVar) {
        PackageInfo packageInfo;
        Bundle bundle;
        ApplicationInfo applicationInfo = this.kI.kL.getApplicationInfo();
        try {
            packageInfo = this.kI.kL.getPackageManager().getPackageInfo(applicationInfo.packageName, 0);
        } catch (NameNotFoundException e) {
            packageInfo = null;
        }
        if (this.kI.kQ.lo || this.kI.kK.getParent() == null) {
            bundle = null;
        } else {
            int[] iArr = new int[2];
            this.kI.kK.getLocationOnScreen(iArr);
            int i = iArr[0];
            int i2 = iArr[1];
            DisplayMetrics displayMetrics = this.kI.kL.getResources().getDisplayMetrics();
            int width = this.kI.kK.getWidth();
            int height = this.kI.kK.getHeight();
            int i3 = (!this.kI.kK.isShown() || i + width <= 0 || i2 + height <= 0 || i > displayMetrics.widthPixels || i2 > displayMetrics.heightPixels) ? 0 : 1;
            bundle = new Bundle(5);
            bundle.putInt("x", i);
            bundle.putInt("y", i2);
            bundle.putInt("width", width);
            bundle.putInt("height", height);
            bundle.putInt("visible", i3);
        }
        String aP = cp.aP();
        this.kI.kS = new co(aP, this.kI.adUnitId);
        this.kI.kS.f(zVar);
        return new com.google.android.gms.internal.cd.a(bundle, zVar, this.kI.kQ, this.kI.adUnitId, applicationInfo, packageInfo, aP, cp.pu, this.kI.kN, cp.a(this.kI, aP, this.kI.kL));
    }

    public void O() {
        af();
    }

    public b P() {
        er.ac("getAdFrame must be called on the main UI thread.");
        return c.h(this.kI.kK);
    }

    public ab Q() {
        er.ac("getAdSize must be called on the main UI thread.");
        return this.kI.kQ;
    }

    public void R() {
        ab();
    }

    public void S() {
        if (this.kI.kQ.lo) {
            ag();
        }
        aa();
        this.kI.kS.aL();
    }

    public void T() {
        if (this.kI.kQ.lo) {
            b(false);
        }
        ac();
    }

    public void U() {
        O();
    }

    public void V() {
        S();
    }

    public void W() {
        R();
    }

    public void X() {
        T();
    }

    public void Y() {
        if (this.kI.kR != null) {
            da.w("Mediation adapter " + this.kI.kR.mO + " refreshed, but mediation adapters should never refresh.");
        }
        b(true);
        ad();
    }

    public void Z() {
        er.ac("recordManualImpression must be called on the main UI thread.");
        if (this.kI.kR == null) {
            da.w("Ad state was null when trying to ping manual tracking URLs.");
            return;
        }
        da.s("Pinging manual tracking URLs.");
        if (this.kI.kR.om != null) {
            cv.a(this.kI.kL, this.kI.kN.pU, this.kI.kR.om);
        }
    }

    public void a(ab abVar) {
        er.ac("setAdSize must be called on the main UI thread.");
        this.kI.kQ = abVar;
        if (this.kI.kR != null) {
            this.kI.kR.nu.a(abVar);
        }
        if (this.kI.kK.getChildCount() > 1) {
            this.kI.kK.removeView(this.kI.kK.getNextView());
        }
        this.kI.kK.setMinimumWidth(abVar.widthPixels);
        this.kI.kK.setMinimumHeight(abVar.heightPixels);
        this.kI.kK.requestLayout();
    }

    public void a(af afVar) {
        er.ac("setAdListener must be called on the main UI thread.");
        this.kI.kO = afVar;
    }

    public void a(ai aiVar) {
        er.ac("setAppEventListener must be called on the main UI thread.");
        this.kI.kT = aiVar;
    }

    public void a(cn cnVar) {
        int i = 0;
        this.kI.kP = null;
        if (!(cnVar.errorCode == -2 || cnVar.errorCode == 3)) {
            cp.a(this.kI);
        }
        if (cnVar.errorCode != -1) {
            boolean z = cnVar.oc.extras != null ? cnVar.oc.extras.getBoolean("_noRefresh", false) : false;
            if (this.kI.kQ.lo) {
                cv.a(cnVar.nu);
            } else if (!z) {
                if (cnVar.mx > 0) {
                    this.kJ.a(cnVar.oc, cnVar.mx);
                } else if (cnVar.pf != null && cnVar.pf.mx > 0) {
                    this.kJ.a(cnVar.oc, cnVar.pf.mx);
                } else if (!cnVar.ok && cnVar.errorCode == 2) {
                    this.kJ.d(cnVar.oc);
                }
            }
            if (!(cnVar.errorCode != 3 || cnVar.pf == null || cnVar.pf.mv == null)) {
                da.s("Pinging no fill URLs.");
                bd.a(this.kI.kL, this.kI.kN.pU, cnVar, this.kI.adUnitId, false, cnVar.pf.mv);
            }
            if (cnVar.errorCode != -2) {
                a(cnVar.errorCode);
            } else if (this.kI.kQ.lo || b(cnVar)) {
                int i2;
                if (!(this.kI.kR == null || this.kI.kR.mP == null)) {
                    this.kI.kR.mP.a(null);
                }
                if (cnVar.mP != null) {
                    cnVar.mP.a((az) this);
                }
                this.kI.kR = cnVar;
                if (cnVar.pg != null) {
                    this.kI.kQ = cnVar.pg;
                }
                this.kI.kS.g(cnVar.ph);
                this.kI.kS.h(cnVar.pi);
                this.kI.kS.k(this.kI.kQ.lo);
                this.kI.kS.l(cnVar.ok);
                if (!this.kI.kQ.lo) {
                    b(false);
                }
                if (this.kI.kU == null) {
                    this.kI.kU = new cr(this.kI.adUnitId);
                }
                if (cnVar.pf != null) {
                    i2 = cnVar.pf.my;
                    i = cnVar.pf.mz;
                } else {
                    i2 = 0;
                }
                this.kI.kU.a(i2, i);
                ad();
            } else {
                a(0);
            }
        }
    }

    public boolean a(z zVar) {
        er.ac("loadAd must be called on the main UI thread.");
        if (this.kI.kP != null) {
            da.w("An ad request is already in progress. Aborting.");
            return false;
        } else if (this.kI.kQ.lo && this.kI.kR != null) {
            da.w("An interstitial is already loading. Aborting.");
            return false;
        } else if (!ae()) {
            return false;
        } else {
            dd ddVar;
            da.u("Starting ad request.");
            this.kJ.cancel();
            com.google.android.gms.internal.cd.a c = c(zVar);
            if (this.kI.kQ.lo) {
                dd a = dd.a(this.kI.kL, this.kI.kQ, false, false, this.kI.kM, this.kI.kN);
                a.bb().a(this, null, this, this, true);
                ddVar = a;
            } else {
                dd ddVar2;
                View nextView = this.kI.kK.getNextView();
                if (nextView instanceof dd) {
                    ddVar2 = (dd) nextView;
                    ddVar2.a(this.kI.kL, this.kI.kQ);
                } else {
                    if (nextView != null) {
                        this.kI.kK.removeView(nextView);
                    }
                    nextView = dd.a(this.kI.kL, this.kI.kQ, false, false, this.kI.kM, this.kI.kN);
                    if (this.kI.kQ.lp == null) {
                        b(nextView);
                    }
                }
                ddVar2.bb().a(this, this, this, this, false);
                ddVar = ddVar2;
            }
            this.kI.kP = by.a(this.kI.kL, c, this.kI.kM, ddVar, this.kH, this);
            return true;
        }
    }

    public void b(z zVar) {
        ViewParent parent = this.kI.kK.getParent();
        if ((parent instanceof View) && ((View) parent).isShown() && cv.aS()) {
            a(zVar);
            return;
        }
        da.u("Ad is not visible. Not refreshing ad.");
        this.kJ.d(zVar);
    }

    public void destroy() {
        er.ac("destroy must be called on the main UI thread.");
        this.kI.kO = null;
        this.kI.kT = null;
        this.kJ.cancel();
        stopLoading();
        if (this.kI.kK != null) {
            this.kI.kK.removeAllViews();
        }
        if (this.kI.kR != null && this.kI.kR.nu != null) {
            this.kI.kR.nu.destroy();
        }
    }

    public boolean isReady() {
        er.ac("isLoaded must be called on the main UI thread.");
        return this.kI.kP == null && this.kI.kR != null;
    }

    public void onAppEvent(String name, String info) {
        if (this.kI.kT != null) {
            try {
                this.kI.kT.onAppEvent(name, info);
            } catch (Throwable e) {
                da.b("Could not call the AppEventListener.", e);
            }
        }
    }

    public void pause() {
        er.ac("pause must be called on the main UI thread.");
        if (this.kI.kR != null) {
            cv.a(this.kI.kR.nu);
        }
    }

    public void resume() {
        er.ac("resume must be called on the main UI thread.");
        if (this.kI.kR != null) {
            cv.b(this.kI.kR.nu);
        }
    }

    public void showInterstitial() {
        er.ac("showInterstitial must be called on the main UI thread.");
        if (!this.kI.kQ.lo) {
            da.w("Cannot call showInterstitial on a banner ad.");
        } else if (this.kI.kR == null) {
            da.w("The interstitial has not loaded.");
        } else if (this.kI.kR.nu.be()) {
            da.w("The interstitial is already showing.");
        } else {
            this.kI.kR.nu.n(true);
            if (this.kI.kR.ok) {
                try {
                    this.kI.kR.mN.showInterstitial();
                    return;
                } catch (Throwable e) {
                    da.b("Could not show interstitial.", e);
                    ag();
                    return;
                }
            }
            bo.a(this.kI.kL, new bq(this, this, this, this.kI.kR.nu, this.kI.kR.orientation, this.kI.kN));
        }
    }

    public void stopLoading() {
        er.ac("stopLoading must be called on the main UI thread.");
        if (this.kI.kR != null) {
            this.kI.kR.nu.stopLoading();
            this.kI.kR = null;
        }
        if (this.kI.kP != null) {
            this.kI.kP.cancel();
        }
    }
}
