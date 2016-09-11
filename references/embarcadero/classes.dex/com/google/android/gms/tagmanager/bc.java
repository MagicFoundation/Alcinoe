package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.a;
import com.google.android.gms.internal.d;
import java.util.Locale;
import java.util.Map;

class bc extends aj {
    private static final String ID;

    static {
        ID = a.LANGUAGE.toString();
    }

    public bc() {
        super(ID, new String[0]);
    }

    public boolean iy() {
        return false;
    }

    public d.a u(Map<String, d.a> map) {
        Locale locale = Locale.getDefault();
        if (locale == null) {
            return di.ku();
        }
        String language = locale.getLanguage();
        return language == null ? di.ku() : di.r(language.toLowerCase());
    }
}
