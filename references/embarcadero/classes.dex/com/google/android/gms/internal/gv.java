package com.google.android.gms.internal;

import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public final class gv {
    public static String aW(int i) {
        switch (i) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                return "TURN_STATUS_INVITED";
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return "TURN_STATUS_MY_TURN";
            case DetectedActivity.ON_FOOT /*2*/:
                return "TURN_STATUS_THEIR_TURN";
            case DetectedActivity.STILL /*3*/:
                return "TURN_STATUS_COMPLETE";
            default:
                fz.h("MatchTurnStatus", "Unknown match turn status: " + i);
                return "TURN_STATUS_UNKNOWN";
        }
    }
}
