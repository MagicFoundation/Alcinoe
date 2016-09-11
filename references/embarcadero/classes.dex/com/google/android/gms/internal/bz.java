package com.google.android.gms.internal;

import android.content.Context;
import android.os.SystemClock;
import android.text.TextUtils;
import com.google.android.vending.licensing.APKExpansionPolicy;
import org.json.JSONException;

public final class bz extends ct implements com.google.android.gms.internal.ca.a, com.google.android.gms.internal.de.a {
    private final bf kH;
    private final Context mContext;
    private final Object mg;
    private ay mh;
    private final com.google.android.gms.internal.by.a nM;
    private final Object nN;
    private final com.google.android.gms.internal.cd.a nO;
    private final l nP;
    private ct nQ;
    private cf nR;
    private boolean nS;
    private aw nT;
    private bc nU;
    private final dd ng;

    /* renamed from: com.google.android.gms.internal.bz.2 */
    class AnonymousClass2 implements Runnable {
        final /* synthetic */ bz nV;
        final /* synthetic */ cn nW;

        AnonymousClass2(bz bzVar, cn cnVar) {
            this.nV = bzVar;
            this.nW = cnVar;
        }

        public void run() {
            synchronized (this.nV.mg) {
                this.nV.nM.a(this.nW);
            }
        }
    }

    private static final class a extends Exception {
        private final int nX;

        public a(String str, int i) {
            super(str);
            this.nX = i;
        }

        public int getErrorCode() {
            return this.nX;
        }
    }

    public bz(Context context, com.google.android.gms.internal.cd.a aVar, l lVar, dd ddVar, bf bfVar, com.google.android.gms.internal.by.a aVar2) {
        this.nN = new Object();
        this.mg = new Object();
        this.nS = false;
        this.kH = bfVar;
        this.nM = aVar2;
        this.ng = ddVar;
        this.mContext = context;
        this.nO = aVar;
        this.nP = lVar;
    }

    private ab a(cd cdVar) throws a {
        if (this.nR.on == null) {
            throw new a("The ad response must specify one of the supported ad sizes.", 0);
        }
        String[] split = this.nR.on.split("x");
        if (split.length != 2) {
            throw new a("Could not parse the ad size from the ad response: " + this.nR.on, 0);
        }
        try {
            int parseInt = Integer.parseInt(split[0]);
            int parseInt2 = Integer.parseInt(split[1]);
            for (ab abVar : cdVar.kQ.lp) {
                float f = this.mContext.getResources().getDisplayMetrics().density;
                int i = abVar.width == -1 ? (int) (((float) abVar.widthPixels) / f) : abVar.width;
                int i2 = abVar.height == -2 ? (int) (((float) abVar.heightPixels) / f) : abVar.height;
                if (parseInt == i && parseInt2 == i2) {
                    return new ab(abVar, cdVar.kQ.lp);
                }
            }
            throw new a("The ad size from the ad response was not one of the requested sizes: " + this.nR.on, 0);
        } catch (NumberFormatException e) {
            throw new a("Could not parse the ad size from the ad response: " + this.nR.on, 0);
        }
    }

    private void a(cd cdVar, long j) throws a {
        synchronized (this.nN) {
            this.nT = new aw(this.mContext, cdVar, this.kH, this.mh);
        }
        this.nU = this.nT.a(j, 60000);
        switch (this.nU.mL) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                throw new a("No fill from any mediation ad networks.", 3);
            default:
                throw new a("Unexpected mediation result: " + this.nU.mL, 0);
        }
    }

    private void aC() throws a {
        if (this.nR.errorCode != -3) {
            if (TextUtils.isEmpty(this.nR.oi)) {
                throw new a("No fill from ad server.", 3);
            } else if (this.nR.ok) {
                try {
                    this.mh = new ay(this.nR.oi);
                } catch (JSONException e) {
                    throw new a("Could not parse mediation config: " + this.nR.oi, 0);
                }
            }
        }
    }

    private void b(long j) throws a {
        cz.pT.post(new Runnable() {
            final /* synthetic */ bz nV;

            {
                this.nV = r1;
            }

            public void run() {
                synchronized (this.nV.mg) {
                    if (this.nV.nR.errorCode != -2) {
                        return;
                    }
                    this.nV.ng.bb().a(this.nV);
                    if (this.nV.nR.errorCode == -3) {
                        da.v("Loading URL in WebView: " + this.nV.nR.nw);
                        this.nV.ng.loadUrl(this.nV.nR.nw);
                    } else {
                        da.v("Loading HTML in WebView.");
                        this.nV.ng.loadDataWithBaseURL(cv.p(this.nV.nR.nw), this.nV.nR.oi, "text/html", "UTF-8", null);
                    }
                }
            }
        });
        d(j);
    }

    private void c(long j) throws a {
        while (e(j)) {
            if (this.nR != null) {
                synchronized (this.nN) {
                    this.nQ = null;
                }
                if (this.nR.errorCode != -2 && this.nR.errorCode != -3) {
                    throw new a("There was a problem getting an ad response. ErrorCode: " + this.nR.errorCode, this.nR.errorCode);
                }
                return;
            }
        }
        throw new a("Timed out waiting for ad response.", 2);
    }

    private void d(long j) throws a {
        while (e(j)) {
            if (this.nS) {
                return;
            }
        }
        throw new a("Timed out waiting for WebView to finish loading.", 2);
    }

    private boolean e(long j) throws a {
        long elapsedRealtime = 60000 - (SystemClock.elapsedRealtime() - j);
        if (elapsedRealtime <= 0) {
            return false;
        }
        try {
            this.mg.wait(elapsedRealtime);
            return true;
        } catch (InterruptedException e) {
            throw new a("Ad request cancelled.", -1);
        }
    }

    public void a(cf cfVar) {
        synchronized (this.mg) {
            da.s("Received ad response.");
            this.nR = cfVar;
            this.mg.notify();
        }
    }

    public void a(dd ddVar) {
        synchronized (this.mg) {
            da.s("WebView finished loading.");
            this.nS = true;
            this.mg.notify();
        }
    }

    public void aB() {
        synchronized (this.mg) {
            long j;
            ab abVar;
            da.s("AdLoaderBackgroundTask started.");
            cd cdVar = new cd(this.nO, this.nP.y().a(this.mContext));
            ab abVar2 = null;
            int i = -2;
            try {
                long elapsedRealtime = SystemClock.elapsedRealtime();
                ct a = ca.a(this.mContext, cdVar, this);
                synchronized (this.nN) {
                    this.nQ = a;
                    if (this.nQ == null) {
                        throw new a("Could not start the ad request service.", 0);
                    }
                }
                c(elapsedRealtime);
                long elapsedRealtime2 = SystemClock.elapsedRealtime();
                aC();
                if (cdVar.kQ.lp != null) {
                    abVar2 = a(cdVar);
                }
                if (this.nR.ok) {
                    a(cdVar, elapsedRealtime);
                } else {
                    b(elapsedRealtime);
                }
                j = elapsedRealtime2;
                abVar = abVar2;
            } catch (a e) {
                i = e.getErrorCode();
                if (i == 3 || i == -1) {
                    da.u(e.getMessage());
                } else {
                    da.w(e.getMessage());
                }
                this.nR = new cf(i);
                cz.pT.post(new Runnable() {
                    final /* synthetic */ bz nV;

                    {
                        this.nV = r1;
                    }

                    public void run() {
                        this.nV.onStop();
                    }
                });
                j = -1;
                abVar = null;
            }
            cz.pT.post(new AnonymousClass2(this, new cn(cdVar.oc, this.ng, this.nR.mt, i, this.nR.mu, this.nR.om, this.nR.orientation, this.nR.mx, cdVar.of, this.nR.ok, this.nU != null ? this.nU.mM : null, this.nU != null ? this.nU.mN : null, this.nU != null ? this.nU.mO : null, this.mh, this.nU != null ? this.nU.mP : null, this.nR.ol, abVar, this.nR.oj, j, this.nR.oo)));
        }
    }

    public void onStop() {
        synchronized (this.nN) {
            if (this.nQ != null) {
                this.nQ.cancel();
            }
            this.ng.stopLoading();
            cv.a(this.ng);
            if (this.nT != null) {
                this.nT.cancel();
            }
        }
    }
}
