package com.google.android.gms.maps.internal;

import android.content.Context;
import android.os.Build.VERSION;
import android.os.IBinder;
import android.os.RemoteException;
import android.util.Log;
import com.google.android.gms.common.GooglePlayServicesNotAvailableException;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.dynamic.c;
import com.google.android.gms.internal.er;
import com.google.android.gms.maps.internal.c.a;
import com.google.android.gms.maps.model.RuntimeRemoteException;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class q {
    private static Context PB;
    private static c PC;

    public static c A(Context context) throws GooglePlayServicesNotAvailableException {
        er.f(context);
        if (PC != null) {
            return PC;
        }
        B(context);
        PC = C(context);
        try {
            PC.a(c.h(getRemoteContext(context).getResources()), (int) GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_VERSION_CODE);
            return PC;
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    private static void B(Context context) throws GooglePlayServicesNotAvailableException {
        int isGooglePlayServicesAvailable = GooglePlayServicesUtil.isGooglePlayServicesAvailable(context);
        switch (isGooglePlayServicesAvailable) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
            default:
                throw new GooglePlayServicesNotAvailableException(isGooglePlayServicesAvailable);
        }
    }

    private static c C(Context context) {
        if (!ha()) {
            return a.U((IBinder) a(getRemoteContext(context).getClassLoader(), "com.google.android.gms.maps.internal.CreatorImpl"));
        }
        Log.i(q.class.getSimpleName(), "Making Creator statically");
        return (c) c(hb());
    }

    private static <T> T a(ClassLoader classLoader, String str) {
        try {
            return c(((ClassLoader) er.f(classLoader)).loadClass(str));
        } catch (ClassNotFoundException e) {
            throw new IllegalStateException("Unable to find dynamic class " + str);
        }
    }

    private static <T> T c(Class<?> cls) {
        try {
            return cls.newInstance();
        } catch (InstantiationException e) {
            throw new IllegalStateException("Unable to instantiate the dynamic class " + cls.getName());
        } catch (IllegalAccessException e2) {
            throw new IllegalStateException("Unable to call the default constructor of " + cls.getName());
        }
    }

    private static Context getRemoteContext(Context context) {
        if (PB == null) {
            if (ha()) {
                PB = context;
            } else {
                PB = GooglePlayServicesUtil.getRemoteContext(context);
            }
        }
        return PB;
    }

    private static boolean ha() {
        return false;
    }

    private static Class<?> hb() {
        try {
            return VERSION.SDK_INT < 15 ? Class.forName("com.google.android.gms.maps.internal.CreatorImplGmm6") : Class.forName("com.google.android.gms.maps.internal.CreatorImpl");
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }
}
