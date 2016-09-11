package com.google.android.gms.internal;

import android.content.Context;
import com.google.android.gms.ads.identifier.AdvertisingIdClient;
import com.google.android.gms.ads.identifier.AdvertisingIdClient.Info;
import com.google.android.gms.common.GooglePlayServicesNotAvailableException;
import java.io.IOException;

public class k extends j {

    class a {
        private String kl;
        private boolean km;
        final /* synthetic */ k kn;

        public a(k kVar, String str, boolean z) {
            this.kn = kVar;
            this.kl = str;
            this.km = z;
        }

        public String getId() {
            return this.kl;
        }

        public boolean isLimitAdTrackingEnabled() {
            return this.km;
        }
    }

    private k(Context context, n nVar, o oVar) {
        super(context, nVar, oVar);
    }

    public static k a(String str, Context context) {
        n eVar = new e();
        j.a(str, context, eVar);
        return new k(context, eVar, new q(239));
    }

    protected void b(Context context) {
        long j = 1;
        super.b(context);
        try {
            a f = f(context);
            try {
                if (!f.isLimitAdTrackingEnabled()) {
                    j = 0;
                }
                a(28, j);
                String id = f.getId();
                if (id != null) {
                    a(30, id);
                }
            } catch (IOException e) {
            }
        } catch (GooglePlayServicesNotAvailableException e2) {
        } catch (IOException e3) {
            a(28, 1);
        }
    }

    a f(Context context) throws IOException, GooglePlayServicesNotAvailableException {
        int i = 0;
        try {
            String str;
            Info advertisingIdInfo = AdvertisingIdClient.getAdvertisingIdInfo(context);
            String id = advertisingIdInfo.getId();
            if (id == null || !id.matches("^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$")) {
                str = id;
            } else {
                byte[] bArr = new byte[16];
                int i2 = 0;
                while (i < id.length()) {
                    if (i == 8 || i == 13 || i == 18 || i == 23) {
                        i++;
                    }
                    bArr[i2] = (byte) ((Character.digit(id.charAt(i), 16) << 4) + Character.digit(id.charAt(i + 1), 16));
                    i2++;
                    i += 2;
                }
                str = this.ka.a(bArr, true);
            }
            return new a(this, str, advertisingIdInfo.isLimitAdTrackingEnabled());
        } catch (Throwable e) {
            throw new IOException(e);
        }
    }
}
