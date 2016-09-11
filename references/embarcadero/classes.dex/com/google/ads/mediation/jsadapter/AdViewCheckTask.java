package com.google.ads.mediation.jsadapter;

import android.graphics.Bitmap;
import android.graphics.Bitmap.Config;
import android.graphics.Canvas;
import android.os.AsyncTask;
import android.os.Handler;
import android.os.Looper;
import android.view.View.MeasureSpec;
import android.webkit.WebView;
import com.google.android.gms.internal.da;

public final class AdViewCheckTask implements Runnable {
    public static final int BACKGROUND_COLOR = 0;
    private final JavascriptAdapter r;
    private final Handler s;
    private final long t;
    private long u;

    private final class a extends AsyncTask<Void, Void, Boolean> {
        private final int v;
        private final int w;
        private final WebView x;
        private Bitmap y;
        final /* synthetic */ AdViewCheckTask z;

        public a(AdViewCheckTask adViewCheckTask, int i, int i2, WebView webView) {
            this.z = adViewCheckTask;
            this.v = i2;
            this.w = i;
            this.x = webView;
        }

        protected synchronized Boolean a(Void... voidArr) {
            Boolean valueOf;
            int width = this.y.getWidth();
            int height = this.y.getHeight();
            if (width == 0 || height == 0) {
                valueOf = Boolean.valueOf(false);
            } else {
                int i = 0;
                for (int i2 = 0; i2 < width; i2 += 10) {
                    for (int i3 = 0; i3 < height; i3 += 10) {
                        if (this.y.getPixel(i2, i3) != 0) {
                            i++;
                        }
                    }
                }
                valueOf = Boolean.valueOf(((double) i) / (((double) (width * height)) / 100.0d) > 0.1d);
            }
            return valueOf;
        }

        protected void a(Boolean bool) {
            AdViewCheckTask.a(this.z);
            if (bool.booleanValue()) {
                this.z.r.sendAdReceivedUpdate();
            } else if (this.z.u > 0) {
                if (da.n(2)) {
                    da.s("Ad not detected, scheduling another run.");
                }
                this.z.s.postDelayed(this.z, this.z.t);
            } else {
                da.s("Ad not detected, Not scheduling anymore runs.");
                this.z.r.sendAdNotReceivedUpdate();
            }
        }

        protected /* synthetic */ Object doInBackground(Object[] x0) {
            return a((Void[]) x0);
        }

        protected /* synthetic */ void onPostExecute(Object x0) {
            a((Boolean) x0);
        }

        protected synchronized void onPreExecute() {
            this.y = Bitmap.createBitmap(this.w, this.v, Config.ARGB_8888);
            this.x.setVisibility(0);
            this.x.measure(MeasureSpec.makeMeasureSpec(this.w, 0), MeasureSpec.makeMeasureSpec(this.v, 0));
            this.x.layout(0, 0, this.w, this.v);
            this.x.draw(new Canvas(this.y));
            this.x.invalidate();
        }
    }

    public AdViewCheckTask(JavascriptAdapter adapter, long checkIntervalInMillis, long numIterations) {
        this.r = adapter;
        this.t = checkIntervalInMillis;
        this.u = numIterations;
        this.s = new Handler(Looper.getMainLooper());
    }

    static /* synthetic */ long a(AdViewCheckTask adViewCheckTask) {
        long j = adViewCheckTask.u - 1;
        adViewCheckTask.u = j;
        return j;
    }

    public void run() {
        if (this.r != null && !this.r.shouldStopAdCheck()) {
            new a(this, this.r.getWebViewWidth(), this.r.getWebViewHeight(), this.r.getWebView()).execute(new Void[0]);
        }
    }

    public void start() {
        this.s.postDelayed(this, this.t);
    }
}
