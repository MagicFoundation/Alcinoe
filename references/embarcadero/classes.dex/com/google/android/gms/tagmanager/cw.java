package com.google.android.gms.tagmanager;

class cw implements cg {
    private long Xe;
    private final long tP;
    private final int tQ;
    private double tR;
    private final Object tT;

    public cw() {
        this(60, 2000);
    }

    public cw(int i, long j) {
        this.tT = new Object();
        this.tQ = i;
        this.tR = (double) this.tQ;
        this.tP = j;
    }

    public boolean cl() {
        boolean z;
        synchronized (this.tT) {
            long currentTimeMillis = System.currentTimeMillis();
            if (this.tR < ((double) this.tQ)) {
                double d = ((double) (currentTimeMillis - this.Xe)) / ((double) this.tP);
                if (d > 0.0d) {
                    this.tR = Math.min((double) this.tQ, d + this.tR);
                }
            }
            this.Xe = currentTimeMillis;
            if (this.tR >= 1.0d) {
                this.tR -= 1.0d;
                z = true;
            } else {
                bh.w("No more tokens available.");
                z = false;
            }
        }
        return z;
    }
}
