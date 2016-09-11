package com.google.android.gms.internal;

import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public abstract class eg implements SafeParcelable {
    private static final Object Br;
    private static ClassLoader Bs;
    private static Integer Bt;
    private boolean Bu;

    static {
        Br = new Object();
        Bs = null;
        Bt = null;
    }

    public eg() {
        this.Bu = false;
    }

    private static boolean a(Class<?> cls) {
        boolean z = false;
        try {
            z = SafeParcelable.NULL.equals(cls.getField("NULL").get(null));
        } catch (NoSuchFieldException e) {
        } catch (IllegalAccessException e2) {
        }
        return z;
    }

    protected static boolean ae(String str) {
        ClassLoader dX = dX();
        if (dX == null) {
            return true;
        }
        try {
            return a(dX.loadClass(str));
        } catch (Exception e) {
            return false;
        }
    }

    protected static ClassLoader dX() {
        ClassLoader classLoader;
        synchronized (Br) {
            classLoader = Bs;
        }
        return classLoader;
    }

    protected static Integer dY() {
        Integer num;
        synchronized (Br) {
            num = Bt;
        }
        return num;
    }

    protected boolean dZ() {
        return this.Bu;
    }
}
