package com.google.android.gms.analytics;

import android.content.Context;

class ai extends k<aj> {

    private static class a implements com.google.android.gms.analytics.k.a<aj> {
        private final aj uJ;

        public a() {
            this.uJ = new aj();
        }

        public void a(String str, int i) {
            if ("ga_sessionTimeout".equals(str)) {
                this.uJ.uM = i;
            } else {
                aa.w("int configuration name not recognized:  " + str);
            }
        }

        public void a(String str, String str2) {
            this.uJ.uQ.put(str, str2);
        }

        public void b(String str, String str2) {
            if ("ga_trackingId".equals(str)) {
                this.uJ.uK = str2;
            } else if ("ga_sampleFrequency".equals(str)) {
                try {
                    this.uJ.uL = Double.parseDouble(str2);
                } catch (NumberFormatException e) {
                    aa.t("Error parsing ga_sampleFrequency value: " + str2);
                }
            } else {
                aa.w("string configuration name not recognized:  " + str);
            }
        }

        public /* synthetic */ j bz() {
            return cA();
        }

        public void c(String str, boolean z) {
            int i = 1;
            aj ajVar;
            if ("ga_autoActivityTracking".equals(str)) {
                ajVar = this.uJ;
                if (!z) {
                    i = 0;
                }
                ajVar.uN = i;
            } else if ("ga_anonymizeIp".equals(str)) {
                ajVar = this.uJ;
                if (!z) {
                    i = 0;
                }
                ajVar.uO = i;
            } else if ("ga_reportUncaughtExceptions".equals(str)) {
                ajVar = this.uJ;
                if (!z) {
                    i = 0;
                }
                ajVar.uP = i;
            } else {
                aa.w("bool configuration name not recognized:  " + str);
            }
        }

        public aj cA() {
            return this.uJ;
        }
    }

    public ai(Context context) {
        super(context, new a());
    }
}
