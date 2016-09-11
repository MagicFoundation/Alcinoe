package com.google.android.gms.analytics;

class z implements ad {
    private final long tP;
    private final int tQ;
    private double tR;
    private long tS;
    private final Object tT;
    private final String tU;

    public z(int i, long j, String str) {
        this.tT = new Object();
        this.tQ = i;
        this.tR = (double) this.tQ;
        this.tP = j;
        this.tU = str;
    }

    public z(String str) {
        this(60, 2000, str);
    }

    public boolean cl() {
        boolean z;
        synchronized (this.tT) {
            long currentTimeMillis = System.currentTimeMillis();
            if (this.tR < ((double) this.tQ)) {
                double d = ((double) (currentTimeMillis - this.tS)) / ((double) this.tP);
                if (d > 0.0d) {
                    this.tR = Math.min((double) this.tQ, d + this.tR);
                }
            }
            this.tS = currentTimeMillis;
            if (this.tR >= 1.0d) {
                this.tR -= 1.0d;
                z = true;
            } else {
                aa.w("Excessive " + this.tU + " detected; call ignored.");
                z = false;
            }
        }
        return z;
    }
}
