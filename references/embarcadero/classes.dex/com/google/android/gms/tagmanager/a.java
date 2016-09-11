package com.google.android.gms.tagmanager;

import android.content.Context;
import android.os.Process;
import com.google.android.gms.ads.identifier.AdvertisingIdClient;
import com.google.android.gms.ads.identifier.AdvertisingIdClient.Info;
import com.google.android.gms.common.GooglePlayServicesNotAvailableException;
import com.google.android.gms.common.GooglePlayServicesRepairableException;
import com.google.android.gms.internal.fl;
import com.google.android.gms.internal.fn;
import java.io.IOException;

class a {
    private static a TA;
    private static Object qI;
    private volatile long Tv;
    private volatile long Tw;
    private volatile long Tx;
    private final fl Ty;
    private a Tz;
    private volatile boolean mClosed;
    private final Context mContext;
    private final Thread pI;
    private volatile Info qK;

    public interface a {
        Info ix();
    }

    static {
        qI = new Object();
    }

    private a(Context context) {
        this(context, null, fn.eI());
    }

    a(Context context, a aVar, fl flVar) {
        this.Tv = 900000;
        this.Tw = 30000;
        this.mClosed = false;
        this.Tz = new a() {
            final /* synthetic */ a TB;

            {
                this.TB = r1;
            }

            public Info ix() {
                Info info = null;
                try {
                    info = AdvertisingIdClient.getAdvertisingIdInfo(this.TB.mContext);
                } catch (IllegalStateException e) {
                    bh.w("IllegalStateException getting Advertising Id Info");
                } catch (GooglePlayServicesRepairableException e2) {
                    bh.w("GooglePlayServicesRepairableException getting Advertising Id Info");
                } catch (IOException e3) {
                    bh.w("IOException getting Ad Id Info");
                } catch (GooglePlayServicesNotAvailableException e4) {
                    bh.w("GooglePlayServicesNotAvailableException getting Advertising Id Info");
                } catch (Exception e5) {
                    bh.w("Unknown exception. Could not get the Advertising Id Info.");
                }
                return info;
            }
        };
        this.Ty = flVar;
        if (context != null) {
            this.mContext = context.getApplicationContext();
        } else {
            this.mContext = context;
        }
        if (aVar != null) {
            this.Tz = aVar;
        }
        this.pI = new Thread(new Runnable() {
            final /* synthetic */ a TB;

            {
                this.TB = r1;
            }

            public void run() {
                this.TB.iv();
            }
        });
    }

    static a E(Context context) {
        if (TA == null) {
            synchronized (qI) {
                if (TA == null) {
                    TA = new a(context);
                    TA.start();
                }
            }
        }
        return TA;
    }

    private void iv() {
        Process.setThreadPriority(10);
        while (!this.mClosed) {
            try {
                this.qK = this.Tz.ix();
                Thread.sleep(this.Tv);
            } catch (InterruptedException e) {
                bh.u("sleep interrupted in AdvertiserDataPoller thread; continuing");
            }
        }
    }

    private void iw() {
        if (this.Ty.currentTimeMillis() - this.Tx >= this.Tw) {
            interrupt();
            this.Tx = this.Ty.currentTimeMillis();
        }
    }

    void interrupt() {
        this.pI.interrupt();
    }

    public boolean isLimitAdTrackingEnabled() {
        iw();
        return this.qK == null ? true : this.qK.isLimitAdTrackingEnabled();
    }

    public String iu() {
        iw();
        return this.qK == null ? null : this.qK.getId();
    }

    void start() {
        this.pI.start();
    }
}
