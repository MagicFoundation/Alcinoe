package com.google.android.gms.tagmanager;

import android.text.TextUtils;

class ap {
    private final long UZ;
    private String Va;
    private final long tL;
    private final long tM;

    ap(long j, long j2, long j3) {
        this.tL = j;
        this.tM = j2;
        this.UZ = j3;
    }

    void F(String str) {
        if (str != null && !TextUtils.isEmpty(str.trim())) {
            this.Va = str;
        }
    }

    long ci() {
        return this.tL;
    }

    long je() {
        return this.UZ;
    }

    String jf() {
        return this.Va;
    }
}
