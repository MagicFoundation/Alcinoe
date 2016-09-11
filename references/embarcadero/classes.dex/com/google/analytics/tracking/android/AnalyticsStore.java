package com.google.analytics.tracking.android;

import com.google.android.gms.analytics.internal.Command;
import java.util.Collection;
import java.util.Map;

interface AnalyticsStore {
    void clearHits(long j);

    void close();

    void dispatch();

    void putHit(Map<String, String> map, long j, String str, Collection<Command> collection);

    void setDispatch(boolean z);
}
