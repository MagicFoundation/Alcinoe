package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.a;
import com.google.android.gms.internal.b;
import com.google.android.gms.internal.d;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

class ch extends aj {
    private static final String ID;
    private static final String Wc;
    private static final String Wd;
    private static final String We;
    private static final String Wf;

    static {
        ID = a.REGEX_GROUP.toString();
        Wc = b.ARG0.toString();
        Wd = b.ARG1.toString();
        We = b.IGNORE_CASE.toString();
        Wf = b.GROUP.toString();
    }

    public ch() {
        super(ID, Wc, Wd);
    }

    public boolean iy() {
        return true;
    }

    public d.a u(Map<String, d.a> map) {
        d.a aVar = (d.a) map.get(Wc);
        d.a aVar2 = (d.a) map.get(Wd);
        if (aVar == null || aVar == di.ku() || aVar2 == null || aVar2 == di.ku()) {
            return di.ku();
        }
        int intValue;
        int i = 64;
        if (di.n((d.a) map.get(We)).booleanValue()) {
            i = 66;
        }
        d.a aVar3 = (d.a) map.get(Wf);
        if (aVar3 != null) {
            Long l = di.l(aVar3);
            if (l == di.kp()) {
                return di.ku();
            }
            intValue = l.intValue();
            if (intValue < 0) {
                return di.ku();
            }
        }
        intValue = 1;
        try {
            CharSequence j = di.j(aVar);
            Object obj = null;
            Matcher matcher = Pattern.compile(di.j(aVar2), i).matcher(j);
            if (matcher.find() && matcher.groupCount() >= intValue) {
                obj = matcher.group(intValue);
            }
            return obj == null ? di.ku() : di.r(obj);
        } catch (PatternSyntaxException e) {
            return di.ku();
        }
    }
}
