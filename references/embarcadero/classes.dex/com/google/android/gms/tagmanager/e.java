package com.google.android.gms.tagmanager;

import android.content.Context;
import com.google.android.gms.internal.a;
import com.google.android.gms.internal.b;
import com.google.android.gms.internal.d;
import java.util.Map;

class e extends aj {
    private static final String ID;
    private static final String TD;
    private static final String TE;
    private final Context kL;

    static {
        ID = a.ADWORDS_CLICK_REFERRER.toString();
        TD = b.COMPONENT.toString();
        TE = b.CONVERSION_ID.toString();
    }

    public e(Context context) {
        super(ID, TE);
        this.kL = context;
    }

    public boolean iy() {
        return true;
    }

    public d.a u(Map<String, d.a> map) {
        d.a aVar = (d.a) map.get(TE);
        if (aVar == null) {
            return di.ku();
        }
        String j = di.j(aVar);
        aVar = (d.a) map.get(TD);
        String e = ay.e(this.kL, j, aVar != null ? di.j(aVar) : null);
        return e != null ? di.r(e) : di.ku();
    }
}
