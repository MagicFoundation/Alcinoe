package com.google.android.gms.tagmanager;

import android.content.Context;
import com.google.android.gms.internal.a;
import com.google.android.gms.internal.d;
import java.util.Map;

class c extends aj {
    private static final String ID;
    private final a TC;

    static {
        ID = a.ADVERTISING_TRACKING_ENABLED.toString();
    }

    public c(Context context) {
        this(a.E(context));
    }

    c(a aVar) {
        super(ID, new String[0]);
        this.TC = aVar;
    }

    public boolean iy() {
        return false;
    }

    public d.a u(Map<String, d.a> map) {
        return di.r(Boolean.valueOf(this.TC.isLimitAdTrackingEnabled()));
    }
}
