package com.google.android.gms.ads.identifier;

import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager.NameNotFoundException;
import android.util.Log;
import com.google.android.gms.common.GooglePlayServicesNotAvailableException;
import com.google.android.gms.common.GooglePlayServicesRepairableException;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.common.a;
import com.google.android.gms.internal.er;
import com.google.android.gms.internal.t;
import java.io.IOException;

public final class AdvertisingIdClient {

    public static final class Info {
        private final String kF;
        private final boolean kG;

        public Info(String advertisingId, boolean limitAdTrackingEnabled) {
            this.kF = advertisingId;
            this.kG = limitAdTrackingEnabled;
        }

        public String getId() {
            return this.kF;
        }

        public boolean isLimitAdTrackingEnabled() {
            return this.kG;
        }
    }

    static Info a(Context context, a aVar) throws IOException {
        try {
            t b = t.a.b(aVar.dm());
            Info info = new Info(b.getId(), b.a(true));
            try {
                context.unbindService(aVar);
            } catch (Throwable e) {
                Log.i("AdvertisingIdClient", "getAdvertisingIdInfo unbindService failed.", e);
            }
            return info;
        } catch (Throwable e2) {
            Log.i("AdvertisingIdClient", "GMS remote exception ", e2);
            throw new IOException("Remote exception");
        } catch (InterruptedException e3) {
            throw new IOException("Interrupted exception");
        } catch (Throwable th) {
            try {
                context.unbindService(aVar);
            } catch (Throwable e4) {
                Log.i("AdvertisingIdClient", "getAdvertisingIdInfo unbindService failed.", e4);
            }
        }
    }

    private static a g(Context context) throws IOException, GooglePlayServicesNotAvailableException, GooglePlayServicesRepairableException {
        try {
            context.getPackageManager().getPackageInfo(GooglePlayServicesUtil.GOOGLE_PLAY_STORE_PACKAGE, 0);
            try {
                GooglePlayServicesUtil.s(context);
                Object aVar = new a();
                Intent intent = new Intent("com.google.android.gms.ads.identifier.service.START");
                intent.setPackage(GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_PACKAGE);
                if (context.bindService(intent, aVar, 1)) {
                    return aVar;
                }
                throw new IOException("Connection failure");
            } catch (Throwable e) {
                throw new IOException(e);
            }
        } catch (NameNotFoundException e2) {
            throw new GooglePlayServicesNotAvailableException(9);
        }
    }

    public static Info getAdvertisingIdInfo(Context context) throws IOException, IllegalStateException, GooglePlayServicesNotAvailableException, GooglePlayServicesRepairableException {
        er.ad("Calling this from your main thread can lead to deadlock");
        return a(context, g(context));
    }
}
