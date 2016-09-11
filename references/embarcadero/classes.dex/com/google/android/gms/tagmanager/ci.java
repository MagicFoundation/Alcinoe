package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.a;
import com.google.android.gms.internal.b;
import com.google.android.gms.internal.d;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

class ci extends dd {
    private static final String ID;
    private static final String We;

    static {
        ID = a.REGEX.toString();
        We = b.IGNORE_CASE.toString();
    }

    public ci() {
        super(ID);
    }

    protected boolean a(String str, String str2, Map<String, d.a> map) {
        try {
            return Pattern.compile(str2, di.n((d.a) map.get(We)).booleanValue() ? 66 : 64).matcher(str).find();
        } catch (PatternSyntaxException e) {
            return false;
        }
    }
}
