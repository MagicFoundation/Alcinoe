package com.google.android.gms.tagmanager;

import android.content.Context;
import com.google.android.gms.internal.a;
import com.google.android.gms.internal.b;
import com.google.android.gms.internal.d;
import java.util.Map;

class ax extends aj {
    private static final String ID;
    private static final String TD;
    private final Context kL;

    static {
        ID = a.INSTALL_REFERRER.toString();
        TD = b.COMPONENT.toString();
    }

    public ax(Context context) {
        super(ID, new String[0]);
        this.kL = context;
    }

    public boolean iy() {
        return true;
    }

    public d.a u(Map<String, d.a> map) {
        String d = ay.d(this.kL, ((d.a) map.get(TD)) != null ? di.j((d.a) map.get(TD)) : null);
        return d != null ? di.r(d) : di.ku();
    }
}
