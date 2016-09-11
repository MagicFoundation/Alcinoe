package com.google.android.gms.internal;

import com.google.android.vending.licensing.APKExpansionPolicy;

public final class gq {
    public static String aW(int i) {
        switch (i) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                return "PUBLIC";
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return "SOCIAL";
            default:
                throw new IllegalArgumentException("Unknown leaderboard collection: " + i);
        }
    }
}
