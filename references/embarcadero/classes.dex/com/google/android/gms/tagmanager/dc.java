package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.a;
import com.google.android.gms.internal.d;
import java.util.Map;

class dc extends dd {
    private static final String ID;

    static {
        ID = a.STARTS_WITH.toString();
    }

    public dc() {
        super(ID);
    }

    protected boolean a(String str, String str2, Map<String, d.a> map) {
        return str.startsWith(str2);
    }
}
