package com.google.android.gms.internal;

import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public final class gr {
    public static boolean isValid(int result) {
        switch (result) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
            case DetectedActivity.ON_FOOT /*2*/:
            case DetectedActivity.STILL /*3*/:
            case DetectedActivity.UNKNOWN /*4*/:
            case DetectedActivity.TILTING /*5*/:
                return true;
            default:
                return false;
        }
    }
}
