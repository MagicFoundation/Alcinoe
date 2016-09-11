package com.google.android.gms.internal;

import android.util.Log;
import com.google.ads.AdRequest;

public final class da {
    public static void a(String str, Throwable th) {
        if (n(3)) {
            Log.d(AdRequest.LOGTAG, str, th);
        }
    }

    public static void b(String str, Throwable th) {
        if (n(5)) {
            Log.w(AdRequest.LOGTAG, str, th);
        }
    }

    public static boolean n(int i) {
        return (i >= 5 || Log.isLoggable(AdRequest.LOGTAG, i)) && i != 2;
    }

    public static void s(String str) {
        if (n(3)) {
            Log.d(AdRequest.LOGTAG, str);
        }
    }

    public static void t(String str) {
        if (n(6)) {
            Log.e(AdRequest.LOGTAG, str);
        }
    }

    public static void u(String str) {
        if (n(4)) {
            Log.i(AdRequest.LOGTAG, str);
        }
    }

    public static void v(String str) {
        if (n(2)) {
            Log.v(AdRequest.LOGTAG, str);
        }
    }

    public static void w(String str) {
        if (n(5)) {
            Log.w(AdRequest.LOGTAG, str);
        }
    }
}
