package com.google.android.gms.analytics;

public class aa {
    private static GoogleAnalytics tV;

    public static boolean cm() {
        return getLogger() != null && getLogger().getLogLevel() == 0;
    }

    private static Logger getLogger() {
        if (tV == null) {
            tV = GoogleAnalytics.cf();
        }
        return tV != null ? tV.getLogger() : null;
    }

    public static void t(String str) {
        Logger logger = getLogger();
        if (logger != null) {
            logger.error(str);
        }
    }

    public static void u(String str) {
        Logger logger = getLogger();
        if (logger != null) {
            logger.info(str);
        }
    }

    public static void v(String str) {
        Logger logger = getLogger();
        if (logger != null) {
            logger.verbose(str);
        }
    }

    public static void w(String str) {
        Logger logger = getLogger();
        if (logger != null) {
            logger.warn(str);
        }
    }
}
