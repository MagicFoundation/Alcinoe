package com.google.android.gms.internal;

import android.os.Build.VERSION;

public final class fr {
    private static boolean ac(int i) {
        return VERSION.SDK_INT >= i;
    }

    public static boolean eJ() {
        return ac(11);
    }

    public static boolean eK() {
        return ac(12);
    }

    public static boolean eL() {
        return ac(13);
    }

    public static boolean eM() {
        return ac(14);
    }

    public static boolean eN() {
        return ac(16);
    }

    public static boolean eO() {
        return ac(17);
    }
}
