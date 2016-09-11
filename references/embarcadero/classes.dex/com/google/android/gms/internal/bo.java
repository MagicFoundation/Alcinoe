package com.google.android.gms.internal;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.Build.VERSION;
import android.os.Bundle;
import android.support.v4.view.ViewCompat;
import android.support.v4.view.accessibility.AccessibilityEventCompat;
import android.support.v4.view.accessibility.AccessibilityNodeInfoCompat;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewGroup.LayoutParams;
import android.view.ViewParent;
import android.view.Window;
import android.webkit.WebChromeClient.CustomViewCallback;
import android.widget.FrameLayout;
import android.widget.RelativeLayout;
import com.google.android.gms.ads.AdActivity;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public final class bo extends com.google.android.gms.internal.bw.a {
    private final Activity nd;
    private bq ne;
    private bs nf;
    private dd ng;
    private b nh;
    private bt ni;
    private FrameLayout nj;
    private CustomViewCallback nk;
    private boolean nl;
    private boolean nm;
    private RelativeLayout nn;

    private static final class a extends Exception {
        public a(String str) {
            super(str);
        }
    }

    private static final class b {
        public final int index;
        public final LayoutParams np;
        public final ViewGroup nq;

        public b(dd ddVar) throws a {
            this.np = ddVar.getLayoutParams();
            ViewParent parent = ddVar.getParent();
            if (parent instanceof ViewGroup) {
                this.nq = (ViewGroup) parent;
                this.index = this.nq.indexOfChild(ddVar);
                this.nq.removeView(ddVar);
                ddVar.n(true);
                return;
            }
            throw new a("Could not get the parent of the WebView for an overlay.");
        }
    }

    public bo(Activity activity) {
        this.nl = false;
        this.nm = false;
        this.nd = activity;
    }

    private static RelativeLayout.LayoutParams a(int i, int i2, int i3, int i4) {
        RelativeLayout.LayoutParams layoutParams = new RelativeLayout.LayoutParams(i3, i4);
        layoutParams.setMargins(i, i2, 0, 0);
        layoutParams.addRule(10);
        layoutParams.addRule(9);
        return layoutParams;
    }

    public static void a(Context context, bq bqVar) {
        Intent intent = new Intent();
        intent.setClassName(context, AdActivity.CLASS_NAME);
        intent.putExtra("com.google.android.gms.ads.internal.overlay.useClientJar", bqVar.kN.pX);
        bq.a(intent, bqVar);
        intent.addFlags(AccessibilityEventCompat.TYPE_GESTURE_DETECTION_END);
        context.startActivity(intent);
    }

    private void as() {
        if (this.nd.isFinishing() && !this.nm) {
            this.nm = true;
            if (this.nd.isFinishing()) {
                if (this.ng != null) {
                    this.ng.aY();
                    this.nn.removeView(this.ng);
                    if (this.nh != null) {
                        this.ng.n(false);
                        this.nh.nq.addView(this.ng, this.nh.index, this.nh.np);
                    }
                }
                if (this.ne != null && this.ne.nt != null) {
                    this.ne.nt.S();
                }
            }
        }
    }

    private void h(boolean z) throws a {
        this.nd.requestWindowFeature(1);
        Window window = this.nd.getWindow();
        window.setFlags(AccessibilityNodeInfoCompat.ACTION_NEXT_HTML_ELEMENT, AccessibilityNodeInfoCompat.ACTION_NEXT_HTML_ELEMENT);
        setRequestedOrientation(this.ne.orientation);
        if (VERSION.SDK_INT >= 11) {
            da.s("Enabling hardware acceleration on the AdActivity window.");
            cw.a(window);
        }
        this.nn = new RelativeLayout(this.nd);
        this.nn.setBackgroundColor(ViewCompat.MEASURED_STATE_MASK);
        this.nd.setContentView(this.nn);
        boolean bi = this.ne.nu.bb().bi();
        if (z) {
            this.ng = dd.a(this.nd, this.ne.nu.Q(), true, bi, null, this.ne.kN);
            this.ng.bb().a(null, null, this.ne.nv, this.ne.nz, true);
            this.ng.bb().a(new com.google.android.gms.internal.de.a() {
                final /* synthetic */ bo no;

                {
                    this.no = r1;
                }

                public void a(dd ddVar) {
                    ddVar.aZ();
                }
            });
            if (this.ne.mZ != null) {
                this.ng.loadUrl(this.ne.mZ);
            } else if (this.ne.ny != null) {
                this.ng.loadDataWithBaseURL(this.ne.nw, this.ne.ny, "text/html", "UTF-8", null);
            } else {
                throw new a("No URL or HTML to display in ad overlay.");
            }
        }
        this.ng = this.ne.nu;
        this.ng.setContext(this.nd);
        this.ng.a(this);
        this.nn.addView(this.ng, -1, -1);
        if (!z) {
            this.ng.aZ();
        }
        f(bi);
    }

    public void a(View view, CustomViewCallback customViewCallback) {
        this.nj = new FrameLayout(this.nd);
        this.nj.setBackgroundColor(ViewCompat.MEASURED_STATE_MASK);
        this.nj.addView(view, -1, -1);
        this.nd.setContentView(this.nj);
        this.nk = customViewCallback;
    }

    public bs ap() {
        return this.nf;
    }

    public void aq() {
        if (this.ne != null) {
            setRequestedOrientation(this.ne.orientation);
        }
        if (this.nj != null) {
            this.nd.setContentView(this.nn);
            this.nj.removeAllViews();
            this.nj = null;
        }
        if (this.nk != null) {
            this.nk.onCustomViewHidden();
            this.nk = null;
        }
    }

    public void ar() {
        this.nn.removeView(this.ni);
        f(true);
    }

    public void b(int i, int i2, int i3, int i4) {
        if (this.nf != null) {
            this.nf.setLayoutParams(a(i, i2, i3, i4));
        }
    }

    public void c(int i, int i2, int i3, int i4) {
        if (this.nf == null) {
            this.nf = new bs(this.nd, this.ng);
            this.nn.addView(this.nf, 0, a(i, i2, i3, i4));
            this.ng.bb().o(false);
        }
    }

    public void close() {
        this.nd.finish();
    }

    public void f(boolean z) {
        this.ni = new bt(this.nd, z ? 50 : 32);
        LayoutParams layoutParams = new RelativeLayout.LayoutParams(-2, -2);
        layoutParams.addRule(10);
        layoutParams.addRule(z ? 11 : 9);
        this.ni.g(this.ne.nx);
        this.nn.addView(this.ni, layoutParams);
    }

    public void g(boolean z) {
        if (this.ni != null) {
            this.ni.g(z);
        }
    }

    public void onCreate(Bundle savedInstanceState) {
        boolean z = false;
        if (savedInstanceState != null) {
            z = savedInstanceState.getBoolean("com.google.android.gms.ads.internal.overlay.hasResumed", false);
        }
        this.nl = z;
        try {
            this.ne = bq.a(this.nd.getIntent());
            if (this.ne == null) {
                throw new a("Could not get info for ad overlay.");
            }
            if (savedInstanceState == null) {
                if (this.ne.nt != null) {
                    this.ne.nt.T();
                }
                if (!(this.ne.nA == 1 || this.ne.ns == null)) {
                    this.ne.ns.O();
                }
            }
            switch (this.ne.nA) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    h(false);
                case DetectedActivity.ON_FOOT /*2*/:
                    this.nh = new b(this.ne.nu);
                    h(false);
                case DetectedActivity.STILL /*3*/:
                    h(true);
                case DetectedActivity.UNKNOWN /*4*/:
                    if (this.nl) {
                        this.nd.finish();
                    } else if (!bl.a(this.nd, this.ne.nr, this.ne.nz)) {
                        this.nd.finish();
                    }
                default:
                    throw new a("Could not determine ad overlay type.");
            }
        } catch (a e) {
            da.w(e.getMessage());
            this.nd.finish();
        }
    }

    public void onDestroy() {
        if (this.nf != null) {
            this.nf.destroy();
        }
        if (this.ng != null) {
            this.nn.removeView(this.ng);
        }
        as();
    }

    public void onPause() {
        if (this.nf != null) {
            this.nf.pause();
        }
        aq();
        if (this.ng != null && (!this.nd.isFinishing() || this.nh == null)) {
            cv.a(this.ng);
        }
        as();
    }

    public void onRestart() {
    }

    public void onResume() {
        if (this.ne != null && this.ne.nA == 4) {
            if (this.nl) {
                this.nd.finish();
            } else {
                this.nl = true;
            }
        }
        if (this.ng != null) {
            cv.b(this.ng);
        }
    }

    public void onSaveInstanceState(Bundle outBundle) {
        outBundle.putBoolean("com.google.android.gms.ads.internal.overlay.hasResumed", this.nl);
    }

    public void onStart() {
    }

    public void onStop() {
        as();
    }

    public void setRequestedOrientation(int requestedOrientation) {
        this.nd.setRequestedOrientation(requestedOrientation);
    }
}
