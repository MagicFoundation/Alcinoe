package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.fl;

class bf implements cg {
    private final fl Ty;
    private final long Vz;
    private final long tP;
    private final int tQ;
    private double tR;
    private long tS;
    private final Object tT;
    private final String tU;

    public bf(int i, long j, long j2, String str, fl flVar) {
        this.tT = new Object();
        this.tQ = i;
        this.tR = (double) this.tQ;
        this.tP = j;
        this.Vz = j2;
        this.tU = str;
        this.Ty = flVar;
    }

    public boolean cl() {
        boolean z = false;
        synchronized (this.tT) {
            long currentTimeMillis = this.Ty.currentTimeMillis();
            if (currentTimeMillis - this.tS < this.Vz) {
                bh.w("Excessive " + this.tU + " detected; call ignored.");
            } else {
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
                    bh.w("Excessive " + this.tU + " detected; call ignored.");
                }
            }
        }
        return z;
    }
}
