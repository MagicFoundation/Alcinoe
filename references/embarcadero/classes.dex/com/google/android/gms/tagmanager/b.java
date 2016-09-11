package com.google.android.gms.tagmanager;

import android.content.Context;
import com.google.android.gms.internal.a;
import com.google.android.gms.internal.d;
import java.util.Map;

class b extends aj {
    private static final String ID;
    private final a TC;

    static {
        ID = a.ADVERTISER_ID.toString();
    }

    public b(Context context) {
        this(a.E(context));
    }

    b(a aVar) {
        super(ID, new String[0]);
        this.TC = aVar;
    }

    public boolean iy() {
        return false;
    }

    public d.a u(Map<String, d.a> map) {
        String iu = this.TC.iu();
        return iu == null ? di.ku() : di.r(iu);
    }
}
