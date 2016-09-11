package com.google.android.gms.tagmanager;

import android.util.Log;

class x implements bi {
    private int rc;

    x() {
        this.rc = 5;
    }

    public void b(String str, Throwable th) {
        if (this.rc <= 5) {
            Log.w("GoogleTagManager", str, th);
        }
    }

    public void c(String str, Throwable th) {
        if (this.rc <= 6) {
            Log.e("GoogleTagManager", str, th);
        }
    }

    public void s(String str) {
        if (this.rc <= 3) {
            Log.d("GoogleTagManager", str);
        }
    }

    public void setLogLevel(int logLevel) {
        this.rc = logLevel;
    }

    public void t(String str) {
        if (this.rc <= 6) {
            Log.e("GoogleTagManager", str);
        }
    }

    public void u(String str) {
        if (this.rc <= 4) {
            Log.i("GoogleTagManager", str);
        }
    }

    public void v(String str) {
        if (this.rc <= 2) {
            Log.v("GoogleTagManager", str);
        }
    }

    public void w(String str) {
        if (this.rc <= 5) {
            Log.w("GoogleTagManager", str);
        }
    }
}
