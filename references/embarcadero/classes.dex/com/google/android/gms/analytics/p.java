package com.google.android.gms.analytics;

import android.os.Build.VERSION;
import java.io.File;

class p {
    static boolean B(String str) {
        if (version() < 9) {
            return false;
        }
        File file = new File(str);
        file.setReadable(false, false);
        file.setWritable(false, false);
        file.setReadable(true, true);
        file.setWritable(true, true);
        return true;
    }

    public static int version() {
        try {
            return Integer.parseInt(VERSION.SDK);
        } catch (NumberFormatException e) {
            aa.t("Invalid version number: " + VERSION.SDK);
            return 0;
        }
    }
}
