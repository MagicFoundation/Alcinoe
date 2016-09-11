package com.google.android.gms.internal;

import android.os.Bundle;

public class cr {
    private final Object mg;
    private int pF;
    private int pG;
    private final String pl;

    public cr(String str) {
        this.mg = new Object();
        this.pl = str;
    }

    public void a(int i, int i2) {
        synchronized (this.mg) {
            this.pF = i;
            this.pG = i2;
            cp.a(this.pl, this);
        }
    }

    public Bundle toBundle() {
        Bundle bundle;
        synchronized (this.mg) {
            bundle = new Bundle();
            bundle.putInt("pmnli", this.pF);
            bundle.putInt("pmnll", this.pG);
        }
        return bundle;
    }
}
