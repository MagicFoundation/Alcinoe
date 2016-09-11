package com.google.android.gms.internal;

import android.content.Context;
import android.os.Bundle;

public class cq {
    private final Object mg;
    private int pA;
    private long pB;
    private long pC;
    private int pD;
    private int pE;
    private final String pz;

    public cq(String str) {
        this.mg = new Object();
        this.pA = 0;
        this.pB = -1;
        this.pC = -1;
        this.pD = 0;
        this.pE = -1;
        this.pz = str;
    }

    public synchronized void aJ() {
        synchronized (this.mg) {
            this.pD++;
        }
    }

    public void aK() {
        synchronized (this.mg) {
            this.pA++;
        }
    }

    public Bundle b(String str, Context context) {
        Bundle bundle;
        synchronized (this.mg) {
            bundle = new Bundle();
            bundle.putString("session_id", this.pz);
            bundle.putLong("basets", this.pC);
            bundle.putLong("currts", this.pB);
            bundle.putString("seq_num", str);
            bundle.putInt("preqs", this.pE);
            bundle.putInt("pclick", this.pA);
            bundle.putInt("pimp", this.pD);
            cm cmVar = new cm(context);
            bundle.putInt("gnt", cmVar.oY);
            if (cmVar.oX == 1) {
                bundle.putString("net", "wi");
            } else {
                bundle.putString("net", "ed");
            }
        }
        return bundle;
    }

    public void b(z zVar, long j) {
        synchronized (this.mg) {
            if (this.pC == -1) {
                this.pC = j;
                this.pB = this.pC;
            } else {
                this.pB = j;
            }
            if (zVar.extras == null || zVar.extras.getInt("gw", 2) != 1) {
                this.pE++;
                return;
            }
        }
    }
}
