package com.google.android.gms.internal;

import android.content.Context;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.internal.cb.b;

public final class ca {

    public interface a {
        void a(cf cfVar);
    }

    public static ct a(Context context, cd cdVar, a aVar) {
        return cdVar.kN.pX ? b(context, cdVar, aVar) : c(context, cdVar, aVar);
    }

    private static ct b(Context context, cd cdVar, a aVar) {
        da.s("Fetching ad response from local ad request service.");
        ct aVar2 = new com.google.android.gms.internal.cb.a(context, cdVar, aVar);
        aVar2.start();
        return aVar2;
    }

    private static ct c(Context context, cd cdVar, a aVar) {
        da.s("Fetching ad response from remote ad request service.");
        if (GooglePlayServicesUtil.isGooglePlayServicesAvailable(context) == 0) {
            return new b(context, cdVar, aVar);
        }
        da.w("Failed to connect to remote ad request service.");
        return null;
    }
}
