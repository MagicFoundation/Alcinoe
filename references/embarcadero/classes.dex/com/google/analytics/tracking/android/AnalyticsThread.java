package com.google.analytics.tracking.android;

import com.google.analytics.tracking.android.GoogleAnalytics.AppOptOutCallback;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;

interface AnalyticsThread {

    public interface ClientIdCallback {
        void reportClientId(String str);
    }

    void dispatch();

    LinkedBlockingQueue<Runnable> getQueue();

    Thread getThread();

    void requestAppOptOut(AppOptOutCallback appOptOutCallback);

    void requestClientId(ClientIdCallback clientIdCallback);

    void sendHit(Map<String, String> map);

    void setAppOptOut(boolean z);
}
