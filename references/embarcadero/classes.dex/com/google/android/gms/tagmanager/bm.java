package com.google.android.gms.tagmanager;

import android.os.Build.VERSION;

class bm {
    bm() {
    }

    int iA() {
        return VERSION.SDK_INT;
    }

    public bl ji() {
        return iA() < 8 ? new av() : new aw();
    }
}
