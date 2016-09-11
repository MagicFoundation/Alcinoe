package com.google.android.gms.appstate;

public final class AppStateStatusCodes {
    public static final int STATUS_CLIENT_RECONNECT_REQUIRED = 2;
    public static final int STATUS_DEVELOPER_ERROR = 7;
    public static final int STATUS_INTERNAL_ERROR = 1;
    public static final int STATUS_INTERRUPTED = 14;
    public static final int STATUS_NETWORK_ERROR_NO_DATA = 4;
    public static final int STATUS_NETWORK_ERROR_OPERATION_DEFERRED = 5;
    public static final int STATUS_NETWORK_ERROR_OPERATION_FAILED = 6;
    public static final int STATUS_NETWORK_ERROR_STALE_DATA = 3;
    public static final int STATUS_OK = 0;
    public static final int STATUS_STATE_KEY_LIMIT_EXCEEDED = 2003;
    public static final int STATUS_STATE_KEY_NOT_FOUND = 2002;
    public static final int STATUS_TIMEOUT = 15;
    public static final int STATUS_WRITE_OUT_OF_DATE_VERSION = 2000;
    public static final int STATUS_WRITE_SIZE_EXCEEDED = 2001;

    private AppStateStatusCodes() {
    }
}
