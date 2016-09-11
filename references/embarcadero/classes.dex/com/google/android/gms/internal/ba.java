package com.google.android.gms.internal;

import com.google.android.gms.internal.bh.a;

public final class ba extends a {
    private bc.a mA;
    private az mB;
    private final Object mg;

    public ba() {
        this.mg = new Object();
    }

    public void O() {
        synchronized (this.mg) {
            if (this.mB != null) {
                this.mB.U();
            }
        }
    }

    public void a(az azVar) {
        synchronized (this.mg) {
            this.mB = azVar;
        }
    }

    public void a(bc.a aVar) {
        synchronized (this.mg) {
            this.mA = aVar;
        }
    }

    public void onAdClosed() {
        synchronized (this.mg) {
            if (this.mB != null) {
                this.mB.V();
            }
        }
    }

    public void onAdFailedToLoad(int error) {
        synchronized (this.mg) {
            if (this.mA != null) {
                this.mA.f(error == 3 ? 1 : 2);
                this.mA = null;
            }
        }
    }

    public void onAdLeftApplication() {
        synchronized (this.mg) {
            if (this.mB != null) {
                this.mB.W();
            }
        }
    }

    public void onAdLoaded() {
        synchronized (this.mg) {
            if (this.mA != null) {
                this.mA.f(0);
                this.mA = null;
                return;
            }
            if (this.mB != null) {
                this.mB.Y();
            }
        }
    }

    public void onAdOpened() {
        synchronized (this.mg) {
            if (this.mB != null) {
                this.mB.X();
            }
        }
    }
}
