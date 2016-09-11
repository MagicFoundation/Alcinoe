package com.google.android.gms.plus.internal;

import android.content.Context;
import android.os.IBinder;
import android.view.View;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.dynamic.c;
import com.google.android.gms.internal.er;
import com.google.android.gms.plus.PlusOneDummyView;

public final class g {
    private static Context PB;
    private static c Rl;

    public static class a extends Exception {
        public a(String str) {
            super(str);
        }
    }

    private static c D(Context context) throws a {
        er.f(context);
        if (Rl == null) {
            if (PB == null) {
                PB = GooglePlayServicesUtil.getRemoteContext(context);
                if (PB == null) {
                    throw new a("Could not get remote context.");
                }
            }
            try {
                Rl = com.google.android.gms.plus.internal.c.a.az((IBinder) PB.getClassLoader().loadClass("com.google.android.gms.plus.plusone.PlusOneButtonCreatorImpl").newInstance());
            } catch (ClassNotFoundException e) {
                throw new a("Could not load creator class.");
            } catch (InstantiationException e2) {
                throw new a("Could not instantiate creator.");
            } catch (IllegalAccessException e3) {
                throw new a("Could not access creator.");
            }
        }
        return Rl;
    }

    public static View a(Context context, int i, int i2, String str, int i3) {
        if (str != null) {
            return (View) c.b(D(context).a(c.h(context), i, i2, str, i3));
        }
        try {
            throw new NullPointerException();
        } catch (Exception e) {
            return new PlusOneDummyView(context, i);
        }
    }
}
