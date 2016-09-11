package com.google.analytics.tracking.android;

class Hit {
    private final long mHitId;
    private String mHitString;
    private final long mHitTime;
    private String mHitUrl;

    String getHitParams() {
        return this.mHitString;
    }

    void setHitString(String hitString) {
        this.mHitString = hitString;
    }

    long getHitId() {
        return this.mHitId;
    }

    long getHitTime() {
        return this.mHitTime;
    }

    Hit(String hitString, long hitId, long hitTime) {
        this.mHitString = hitString;
        this.mHitId = hitId;
        this.mHitTime = hitTime;
    }

    String getHitUrl() {
        return this.mHitUrl;
    }

    void setHitUrl(String hitUrl) {
        this.mHitUrl = hitUrl;
    }
}
