package com.google.android.gms.analytics;

import java.util.Map;

abstract class TrackerHandler {
    TrackerHandler() {
    }

    abstract void n(Map<String, String> map);
}
