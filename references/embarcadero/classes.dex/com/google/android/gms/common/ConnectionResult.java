package com.google.android.gms.common;

import android.app.Activity;
import android.app.PendingIntent;
import android.content.IntentSender.SendIntentException;
import com.google.android.gms.internal.ep;

public final class ConnectionResult {
    public static final int CANCELED = 13;
    public static final int DATE_INVALID = 12;
    public static final int DEVELOPER_ERROR = 10;
    public static final int DRIVE_EXTERNAL_STORAGE_REQUIRED = 1500;
    public static final int INTERNAL_ERROR = 8;
    public static final int INTERRUPTED = 15;
    public static final int INVALID_ACCOUNT = 5;
    public static final int LICENSE_CHECK_FAILED = 11;
    public static final int NETWORK_ERROR = 7;
    public static final int RESOLUTION_REQUIRED = 6;
    public static final int SERVICE_DISABLED = 3;
    public static final int SERVICE_INVALID = 9;
    public static final int SERVICE_MISSING = 1;
    public static final int SERVICE_VERSION_UPDATE_REQUIRED = 2;
    public static final int SIGN_IN_REQUIRED = 4;
    public static final int SUCCESS = 0;
    public static final int TIMEOUT = 14;
    public static final ConnectionResult yI;
    private final PendingIntent mPendingIntent;
    private final int yJ;

    static {
        yI = new ConnectionResult(SUCCESS, null);
    }

    public ConnectionResult(int statusCode, PendingIntent pendingIntent) {
        this.yJ = statusCode;
        this.mPendingIntent = pendingIntent;
    }

    private String dn() {
        switch (this.yJ) {
            case SUCCESS /*0*/:
                return "SUCCESS";
            case SERVICE_MISSING /*1*/:
                return "SERVICE_MISSING";
            case SERVICE_VERSION_UPDATE_REQUIRED /*2*/:
                return "SERVICE_VERSION_UPDATE_REQUIRED";
            case SERVICE_DISABLED /*3*/:
                return "SERVICE_DISABLED";
            case SIGN_IN_REQUIRED /*4*/:
                return "SIGN_IN_REQUIRED";
            case INVALID_ACCOUNT /*5*/:
                return "INVALID_ACCOUNT";
            case RESOLUTION_REQUIRED /*6*/:
                return "RESOLUTION_REQUIRED";
            case NETWORK_ERROR /*7*/:
                return "NETWORK_ERROR";
            case INTERNAL_ERROR /*8*/:
                return "INTERNAL_ERROR";
            case SERVICE_INVALID /*9*/:
                return "SERVICE_INVALID";
            case DEVELOPER_ERROR /*10*/:
                return "DEVELOPER_ERROR";
            case LICENSE_CHECK_FAILED /*11*/:
                return "LICENSE_CHECK_FAILED";
            case CANCELED /*13*/:
                return "CANCELED";
            case TIMEOUT /*14*/:
                return "TIMEOUT";
            case INTERRUPTED /*15*/:
                return "INTERRUPTED";
            default:
                return "unknown status code " + this.yJ;
        }
    }

    public int getErrorCode() {
        return this.yJ;
    }

    public PendingIntent getResolution() {
        return this.mPendingIntent;
    }

    public boolean hasResolution() {
        return (this.yJ == 0 || this.mPendingIntent == null) ? false : true;
    }

    public boolean isSuccess() {
        return this.yJ == 0;
    }

    public void startResolutionForResult(Activity activity, int requestCode) throws SendIntentException {
        if (hasResolution()) {
            activity.startIntentSenderForResult(this.mPendingIntent.getIntentSender(), requestCode, null, SUCCESS, SUCCESS, SUCCESS);
        }
    }

    public String toString() {
        return ep.e(this).a("statusCode", dn()).a("resolution", this.mPendingIntent).toString();
    }
}
