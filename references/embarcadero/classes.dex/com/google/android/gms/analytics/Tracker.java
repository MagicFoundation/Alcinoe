package com.google.android.gms.analytics;

import android.app.Activity;
import android.content.Context;
import android.text.TextUtils;
import com.google.analytics.tracking.android.ModelFields;
import com.google.android.gms.internal.er;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

public class Tracker {
    private final TrackerHandler up;
    private final Map<String, String> uq;
    private ad ur;
    private final h us;
    private final ae ut;
    private final g uu;
    private boolean uv;
    private a uw;
    private aj ux;

    private class a implements a {
        private i rJ;
        private boolean uA;
        private boolean uB;
        private int uC;
        private long uD;
        private boolean uE;
        private long uF;
        final /* synthetic */ Tracker uG;
        private Timer uy;
        private TimerTask uz;

        private class a extends TimerTask {
            final /* synthetic */ a uI;

            private a(a aVar) {
                this.uI = aVar;
            }

            public void run() {
                this.uI.uA = false;
            }
        }

        /* renamed from: com.google.android.gms.analytics.Tracker.a.1 */
        class AnonymousClass1 implements i {
            final /* synthetic */ Tracker uH;
            final /* synthetic */ a uI;

            AnonymousClass1(a aVar, Tracker tracker) {
                this.uI = aVar;
                this.uH = tracker;
            }

            public long currentTimeMillis() {
                return System.currentTimeMillis();
            }
        }

        public a(Tracker tracker) {
            this.uG = tracker;
            this.uA = false;
            this.uB = false;
            this.uC = 0;
            this.uD = -1;
            this.uE = false;
            this.rJ = new AnonymousClass1(this, tracker);
        }

        private void cx() {
            GoogleAnalytics cf = GoogleAnalytics.cf();
            if (cf == null) {
                aa.t("GoogleAnalytics isn't initialized for the Tracker!");
            } else if (this.uD >= 0 || this.uB) {
                cf.a(this.uG.uw);
            } else {
                cf.b(this.uG.uw);
            }
        }

        private synchronized void cy() {
            if (this.uy != null) {
                this.uy.cancel();
                this.uy = null;
            }
        }

        public long cu() {
            return this.uD;
        }

        public boolean cv() {
            return this.uB;
        }

        public boolean cw() {
            boolean z = this.uE;
            this.uE = false;
            return z;
        }

        boolean cz() {
            return this.uD == 0 || (this.uD > 0 && this.rJ.currentTimeMillis() > this.uF + this.uD);
        }

        public void enableAutoActivityTracking(boolean enabled) {
            this.uB = enabled;
            cx();
        }

        public void f(Activity activity) {
            u.bR().a(com.google.android.gms.analytics.u.a.EASY_TRACKER_ACTIVITY_START);
            cy();
            if (!this.uA && this.uC == 0 && cz()) {
                this.uE = true;
            }
            this.uA = true;
            this.uC++;
            if (this.uB) {
                Map hashMap = new HashMap();
                hashMap.put("&t", ModelFields.APP_VIEW);
                u.bR().r(true);
                this.uG.set("&cd", this.uG.ux != null ? this.uG.ux.h(activity) : activity.getClass().getCanonicalName());
                this.uG.send(hashMap);
                u.bR().r(false);
            }
        }

        public void g(Activity activity) {
            u.bR().a(com.google.android.gms.analytics.u.a.EASY_TRACKER_ACTIVITY_STOP);
            this.uC--;
            this.uC = Math.max(0, this.uC);
            this.uF = this.rJ.currentTimeMillis();
            if (this.uC == 0) {
                cy();
                this.uz = new a();
                this.uy = new Timer("waitForActivityStart");
                this.uy.schedule(this.uz, 1000);
            }
        }

        public void setSessionTimeout(long sessionTimeout) {
            this.uD = sessionTimeout;
            cx();
        }
    }

    Tracker(String trackingId, TrackerHandler handler) {
        this(trackingId, handler, h.bu(), ae.cs(), g.bt(), new z("tracking"));
    }

    Tracker(String trackingId, TrackerHandler handler, h clientIdDefaultProvider, ae screenResolutionDefaultProvider, g appFieldsDefaultProvider, ad rateLimiter) {
        this.uq = new HashMap();
        this.up = handler;
        if (trackingId != null) {
            this.uq.put("&tid", trackingId);
        }
        this.uq.put(ModelFields.USE_SECURE, "1");
        this.us = clientIdDefaultProvider;
        this.ut = screenResolutionDefaultProvider;
        this.uu = appFieldsDefaultProvider;
        this.ur = rateLimiter;
        this.uw = new a(this);
    }

    void a(Context context, aj ajVar) {
        aa.v("Loading Tracker config values.");
        this.ux = ajVar;
        if (this.ux.cB()) {
            String cC = this.ux.cC();
            set("&tid", cC);
            aa.v("[Tracker] trackingId loaded: " + cC);
        }
        if (this.ux.cD()) {
            cC = Double.toString(this.ux.cE());
            set("&sf", cC);
            aa.v("[Tracker] sample frequency loaded: " + cC);
        }
        if (this.ux.cF()) {
            setSessionTimeout((long) this.ux.getSessionTimeout());
            aa.v("[Tracker] session timeout loaded: " + cu());
        }
        if (this.ux.cG()) {
            enableAutoActivityTracking(this.ux.cH());
            aa.v("[Tracker] auto activity tracking loaded: " + cv());
        }
        if (this.ux.cI()) {
            if (this.ux.cJ()) {
                set("&aip", "1");
                aa.v("[Tracker] anonymize ip loaded: true");
            }
            aa.v("[Tracker] anonymize ip loaded: false");
        }
        this.uv = this.ux.cK();
        if (this.ux.cK()) {
            Thread.setDefaultUncaughtExceptionHandler(new ExceptionReporter(this, Thread.getDefaultUncaughtExceptionHandler(), context));
            aa.v("[Tracker] report uncaught exceptions loaded: " + this.uv);
        }
    }

    long cu() {
        return this.uw.cu();
    }

    boolean cv() {
        return this.uw.cv();
    }

    public void enableAdvertisingIdCollection(boolean enabled) {
        if (enabled) {
            if (this.uq.containsKey("&ate")) {
                this.uq.remove("&ate");
            }
            if (this.uq.containsKey("&adid")) {
                this.uq.remove("&adid");
                return;
            }
            return;
        }
        this.uq.put("&ate", null);
        this.uq.put("&adid", null);
    }

    public void enableAutoActivityTracking(boolean enabled) {
        this.uw.enableAutoActivityTracking(enabled);
    }

    public String get(String key) {
        u.bR().a(com.google.android.gms.analytics.u.a.GET);
        if (TextUtils.isEmpty(key)) {
            return null;
        }
        if (this.uq.containsKey(key)) {
            return (String) this.uq.get(key);
        }
        if (key.equals("&ul")) {
            return ak.a(Locale.getDefault());
        }
        if (this.us != null && this.us.x(key)) {
            return this.us.getValue(key);
        }
        if (this.ut == null || !this.ut.x(key)) {
            return (this.uu == null || !this.uu.x(key)) ? null : this.uu.getValue(key);
        } else {
            return this.ut.getValue(key);
        }
    }

    public void send(Map<String, String> params) {
        u.bR().a(com.google.android.gms.analytics.u.a.SEND);
        Map hashMap = new HashMap();
        hashMap.putAll(this.uq);
        if (params != null) {
            hashMap.putAll(params);
        }
        if (TextUtils.isEmpty((CharSequence) hashMap.get("&tid"))) {
            aa.w(String.format("Missing tracking id (%s) parameter.", new Object[]{"&tid"}));
        }
        String str = (String) hashMap.get("&t");
        if (TextUtils.isEmpty(str)) {
            aa.w(String.format("Missing hit type (%s) parameter.", new Object[]{"&t"}));
            str = "";
        }
        if (this.uw.cw()) {
            hashMap.put("&sc", "start");
        }
        if (str.equals("transaction") || str.equals(ModelFields.ITEM) || this.ur.cl()) {
            this.up.n(hashMap);
        } else {
            aa.w("Too many hits sent too quickly, rate limiting invoked.");
        }
    }

    public void set(String key, String value) {
        er.b((Object) key, (Object) "Key should be non-null");
        u.bR().a(com.google.android.gms.analytics.u.a.SET);
        this.uq.put(key, value);
    }

    public void setAnonymizeIp(boolean anonymize) {
        set("&aip", ak.s(anonymize));
    }

    public void setAppId(String appId) {
        set("&aid", appId);
    }

    public void setAppInstallerId(String appInstallerId) {
        set("&aiid", appInstallerId);
    }

    public void setAppName(String appName) {
        set("&an", appName);
    }

    public void setAppVersion(String appVersion) {
        set("&av", appVersion);
    }

    public void setClientId(String clientId) {
        set("&cid", clientId);
    }

    public void setEncoding(String encoding) {
        set("&de", encoding);
    }

    public void setHostname(String hostname) {
        set("&dh", hostname);
    }

    public void setLanguage(String language) {
        set("&ul", language);
    }

    public void setLocation(String location) {
        set("&dl", location);
    }

    public void setPage(String page) {
        set("&dp", page);
    }

    public void setReferrer(String referrer) {
        set("&dr", referrer);
    }

    public void setSampleRate(double sampleRate) {
        set("&sf", Double.toHexString(sampleRate));
    }

    public void setScreenColors(String screenColors) {
        set("&sd", screenColors);
    }

    public void setScreenName(String screenName) {
        set("&cd", screenName);
    }

    public void setScreenResolution(int width, int height) {
        if (width >= 0 || height >= 0) {
            set("&sr", width + "x" + height);
        } else {
            aa.w("Invalid width or height. The values should be non-negative.");
        }
    }

    public void setSessionTimeout(long sessionTimeout) {
        this.uw.setSessionTimeout(1000 * sessionTimeout);
    }

    public void setTitle(String title) {
        set("&dt", title);
    }

    public void setUseSecure(boolean useSecure) {
        set(ModelFields.USE_SECURE, ak.s(useSecure));
    }

    public void setViewportSize(String viewportSize) {
        set("&vp", viewportSize);
    }
}
