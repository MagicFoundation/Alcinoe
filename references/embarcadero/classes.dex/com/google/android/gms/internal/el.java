package com.google.android.gms.internal;

import android.util.Log;

public final class el {
    private final String Cd;

    public el(String str) {
        this.Cd = (String) er.f(str);
    }

    public boolean Q(int i) {
        return Log.isLoggable(this.Cd, i);
    }

    public void a(String str, String str2, Throwable th) {
        if (Q(6)) {
            Log.e(str, str2, th);
        }
    }

    public void f(String str, String str2) {
        if (Q(2)) {
            Log.v(str, str2);
        }
    }

    public void g(String str, String str2) {
        if (Q(5)) {
            Log.w(str, str2);
        }
    }

    public void h(String str, String str2) {
        if (Q(6)) {
            Log.e(str, str2);
        }
    }
}
