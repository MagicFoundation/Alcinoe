package com.google.android.gms.tagmanager;

import android.content.Context;
import com.google.android.gms.internal.a;
import com.google.android.gms.internal.d;
import java.util.Map;

class f extends aj {
    private static final String ID;
    private final Context mContext;

    static {
        ID = a.APP_ID.toString();
    }

    public f(Context context) {
        super(ID, new String[0]);
        this.mContext = context;
    }

    public boolean iy() {
        return true;
    }

    public d.a u(Map<String, d.a> map) {
        return di.r(this.mContext.getPackageName());
    }
}
