package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.a;
import com.google.android.gms.internal.d;
import java.util.Map;

class ad extends dd {
    private static final String ID;

    static {
        ID = a.ENDS_WITH.toString();
    }

    public ad() {
        super(ID);
    }

    protected boolean a(String str, String str2, Map<String, d.a> map) {
        return str.endsWith(str2);
    }
}
