package com.google.android.gms.analytics;

public interface Logger {

    public static class LogLevel {
        public static final int ERROR = 3;
        public static final int INFO = 1;
        public static final int VERBOSE = 0;
        public static final int WARNING = 2;
    }

    void error(Exception exception);

    void error(String str);

    int getLogLevel();

    void info(String str);

    void setLogLevel(int i);

    void verbose(String str);

    void warn(String str);
}
