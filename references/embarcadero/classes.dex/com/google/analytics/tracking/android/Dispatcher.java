package com.google.analytics.tracking.android;

import java.util.List;

interface Dispatcher {
    int dispatchHits(List<Hit> list);

    boolean okToDispatch();
}
