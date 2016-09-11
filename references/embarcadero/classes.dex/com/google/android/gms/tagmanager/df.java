package com.google.android.gms.tagmanager;

import android.content.Context;
import com.google.android.gms.analytics.GoogleAnalytics;
import com.google.android.gms.analytics.Logger;
import com.google.android.gms.analytics.Tracker;
import com.google.android.gms.location.DetectedActivity;

class df {
    private GoogleAnalytics XE;
    private Context mContext;
    private Tracker re;

    static class a implements Logger {
        a() {
        }

        private static int bX(int i) {
            switch (i) {
                case DetectedActivity.ON_FOOT /*2*/:
                    return 0;
                case DetectedActivity.STILL /*3*/:
                case DetectedActivity.UNKNOWN /*4*/:
                    return 1;
                case DetectedActivity.TILTING /*5*/:
                    return 2;
                default:
                    return 3;
            }
        }

        public void error(Exception exception) {
            bh.c("", exception);
        }

        public void error(String message) {
            bh.t(message);
        }

        public int getLogLevel() {
            return bX(bh.getLogLevel());
        }

        public void info(String message) {
            bh.u(message);
        }

        public void setLogLevel(int logLevel) {
            bh.w("GA uses GTM logger. Please use TagManager.setLogLevel(int) instead.");
        }

        public void verbose(String message) {
            bh.v(message);
        }

        public void warn(String message) {
            bh.w(message);
        }
    }

    df(Context context) {
        this.mContext = context;
    }

    private synchronized void bG(String str) {
        if (this.XE == null) {
            this.XE = GoogleAnalytics.getInstance(this.mContext);
            this.XE.setLogger(new a());
            this.re = this.XE.newTracker(str);
        }
    }

    public Tracker bF(String str) {
        bG(str);
        return this.re;
    }
}
