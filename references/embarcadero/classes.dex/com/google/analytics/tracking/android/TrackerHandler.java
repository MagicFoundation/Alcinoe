package com.google.analytics.tracking.android;

import java.util.Map;

interface TrackerHandler {
    void closeTracker(Tracker tracker);

    void sendHit(Map<String, String> map);
}
