package com.google.analytics.tracking.android;

import com.google.android.gms.common.util.VisibleForTesting;

class AdHitIdGenerator {
    private boolean mAdMobSdkInstalled;

    AdHitIdGenerator() {
        try {
            boolean z;
            if (Class.forName("com.google.ads.AdRequest") != null) {
                z = true;
            } else {
                z = false;
            }
            this.mAdMobSdkInstalled = z;
        } catch (ClassNotFoundException e) {
            this.mAdMobSdkInstalled = false;
        }
    }

    @VisibleForTesting
    AdHitIdGenerator(boolean adMobSdkInstalled) {
        this.mAdMobSdkInstalled = adMobSdkInstalled;
    }

    int getAdHitId() {
        if (this.mAdMobSdkInstalled) {
            return AdMobInfo.getInstance().generateAdHitId();
        }
        return 0;
    }
}
