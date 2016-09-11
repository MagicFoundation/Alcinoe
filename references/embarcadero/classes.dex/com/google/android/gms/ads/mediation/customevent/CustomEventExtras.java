package com.google.android.gms.ads.mediation.customevent;

import com.google.ads.mediation.NetworkExtras;
import java.util.HashMap;

public final class CustomEventExtras implements NetworkExtras {
    private final HashMap<String, Object> qt;

    public CustomEventExtras() {
        this.qt = new HashMap();
    }

    public Object getExtra(String label) {
        return this.qt.get(label);
    }

    public void setExtra(String label, Object value) {
        this.qt.put(label, value);
    }
}
