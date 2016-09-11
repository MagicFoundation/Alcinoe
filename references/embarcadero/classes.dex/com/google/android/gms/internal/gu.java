package com.google.android.gms.internal;

import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public final class gu {
    public static String aW(int i) {
        switch (i) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                return "DAILY";
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return "WEEKLY";
            case DetectedActivity.ON_FOOT /*2*/:
                return "ALL_TIME";
            default:
                throw new IllegalArgumentException("Unknown time span " + i);
        }
    }
}
