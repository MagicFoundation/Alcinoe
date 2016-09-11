package com.google.android.gms.analytics;

import android.content.Context;

class v extends k<w> {

    private static class a implements com.google.android.gms.analytics.k.a<w> {
        private final w tx;

        public a() {
            this.tx = new w();
        }

        public void a(String str, int i) {
            if ("ga_dispatchPeriod".equals(str)) {
                this.tx.tz = i;
            } else {
                aa.w("int configuration name not recognized:  " + str);
            }
        }

        public void a(String str, String str2) {
        }

        public void b(String str, String str2) {
            if ("ga_appName".equals(str)) {
                this.tx.qR = str2;
            } else if ("ga_appVersion".equals(str)) {
                this.tx.qS = str2;
            } else if ("ga_logLevel".equals(str)) {
                this.tx.ty = str2;
            } else {
                aa.w("string configuration name not recognized:  " + str);
            }
        }

        public w bU() {
            return this.tx;
        }

        public /* synthetic */ j bz() {
            return bU();
        }

        public void c(String str, boolean z) {
            if ("ga_dryRun".equals(str)) {
                this.tx.tA = z ? 1 : 0;
                return;
            }
            aa.w("bool configuration name not recognized:  " + str);
        }
    }

    public v(Context context) {
        super(context, new a());
    }
}
