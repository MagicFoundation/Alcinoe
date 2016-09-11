package com.google.analytics.tracking.android;

public interface ServiceManager {
    void dispatch();

    void setDispatchPeriod(int i);

    void updateConnectivityStatus(boolean z);
}
