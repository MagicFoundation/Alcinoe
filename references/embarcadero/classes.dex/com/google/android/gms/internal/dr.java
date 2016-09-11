package com.google.android.gms.internal;

public final class dr {
    public static <T> boolean a(T t, T t2) {
        return (t == null && t2 == null) || !(t == null || t2 == null || !t.equals(t2));
    }

    public static long b(double d) {
        return (long) (1000.0d * d);
    }

    public static double l(long j) {
        return ((double) j) / 1000.0d;
    }
}
