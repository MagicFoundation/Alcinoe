package com.google.android.gms.internal;

import android.content.Context;
import android.os.IBinder;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.dynamic.c;
import com.google.android.gms.dynamic.e;
import com.google.android.gms.internal.ag.a;

public final class y extends e<ah> {
    private static final y ld;

    static {
        ld = new y();
    }

    private y() {
        super("com.google.android.gms.ads.AdManagerCreatorImpl");
    }

    public static ag a(Context context, ab abVar, String str, be beVar) {
        if (GooglePlayServicesUtil.isGooglePlayServicesAvailable(context) == 0) {
            ag b = ld.b(context, abVar, str, beVar);
            if (b != null) {
                return b;
            }
        }
        da.s("Using AdManager from the client jar.");
        return new v(context, abVar, str, beVar, new db(GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_VERSION_CODE, GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_VERSION_CODE, true));
    }

    private ag b(Context context, ab abVar, String str, be beVar) {
        try {
            return a.f(((ah) z(context)).a(c.h(context), abVar, str, beVar, GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_VERSION_CODE));
        } catch (Throwable e) {
            da.b("Could not create remote AdManager.", e);
            return null;
        } catch (Throwable e2) {
            da.b("Could not create remote AdManager.", e2);
            return null;
        }
    }

    protected ah c(IBinder iBinder) {
        return ah.a.g(iBinder);
    }

    protected /* synthetic */ Object d(IBinder iBinder) {
        return c(iBinder);
    }
}
