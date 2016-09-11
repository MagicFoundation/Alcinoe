package com.google.android.gms.maps.internal;

import com.google.android.vending.licensing.APKExpansionPolicy;

public final class a {
    public static Boolean a(byte b) {
        switch (b) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                return Boolean.FALSE;
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return Boolean.TRUE;
            default:
                return null;
        }
    }

    public static byte c(Boolean bool) {
        return bool != null ? bool.booleanValue() ? (byte) 1 : (byte) 0 : (byte) -1;
    }
}
