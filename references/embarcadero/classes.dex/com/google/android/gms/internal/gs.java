package com.google.android.gms.internal;

import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public final class gs {
    public static String aW(int i) {
        switch (i) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return "GIFT";
            case DetectedActivity.ON_FOOT /*2*/:
                return "WISH";
            default:
                fz.h("RequestType", "Unknown request type: " + i);
                return "UNKNOWN_TYPE";
        }
    }
}
