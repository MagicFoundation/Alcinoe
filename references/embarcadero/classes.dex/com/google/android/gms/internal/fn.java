package com.google.android.gms.internal;

public final class fn implements fl {
    private static fn CN;

    public static synchronized fl eI() {
        fl flVar;
        synchronized (fn.class) {
            if (CN == null) {
                CN = new fn();
            }
            flVar = CN;
        }
        return flVar;
    }

    public long currentTimeMillis() {
        return System.currentTimeMillis();
    }
}
