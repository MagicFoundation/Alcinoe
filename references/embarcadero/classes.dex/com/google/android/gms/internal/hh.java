package com.google.android.gms.internal;

import android.app.PendingIntent;
import android.content.ContentProviderClient;
import android.content.Context;
import android.location.Location;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.util.Log;
import com.google.android.gms.location.LocationListener;
import com.google.android.gms.location.LocationRequest;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.HashMap;

public class hh {
    private final hl<hg> Lk;
    private ContentProviderClient Ll;
    private boolean Lm;
    private HashMap<LocationListener, b> Ln;
    private final Context mContext;

    private static class a extends Handler {
        private final LocationListener Lo;

        public a(LocationListener locationListener) {
            this.Lo = locationListener;
        }

        public a(LocationListener locationListener, Looper looper) {
            super(looper);
            this.Lo = locationListener;
        }

        public void handleMessage(Message msg) {
            switch (msg.what) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    this.Lo.onLocationChanged(new Location((Location) msg.obj));
                default:
                    Log.e("LocationClientHelper", "unknown message in LocationHandler.handleMessage");
            }
        }
    }

    private static class b extends com.google.android.gms.location.a.a {
        private Handler Lp;

        b(LocationListener locationListener, Looper looper) {
            this.Lp = looper == null ? new a(locationListener) : new a(locationListener, looper);
        }

        public void onLocationChanged(Location location) {
            if (this.Lp == null) {
                Log.e("LocationClientHelper", "Received a location in client after calling removeLocationUpdates.");
                return;
            }
            Message obtain = Message.obtain();
            obtain.what = 1;
            obtain.obj = location;
            this.Lp.sendMessage(obtain);
        }

        public void release() {
            this.Lp = null;
        }
    }

    public hh(Context context, hl<hg> hlVar) {
        this.Ll = null;
        this.Lm = false;
        this.Ln = new HashMap();
        this.mContext = context;
        this.Lk = hlVar;
    }

    public Location getLastLocation() {
        this.Lk.bm();
        try {
            return ((hg) this.Lk.eb()).aF(this.mContext.getPackageName());
        } catch (Throwable e) {
            throw new IllegalStateException(e);
        }
    }

    public void gl() {
        if (this.Lm) {
            setMockMode(false);
        }
    }

    public void removeAllListeners() {
        try {
            synchronized (this.Ln) {
                for (com.google.android.gms.location.a aVar : this.Ln.values()) {
                    if (aVar != null) {
                        ((hg) this.Lk.eb()).a(aVar);
                    }
                }
                this.Ln.clear();
            }
        } catch (Throwable e) {
            throw new IllegalStateException(e);
        }
    }

    public void removeLocationUpdates(PendingIntent callbackIntent) {
        this.Lk.bm();
        try {
            ((hg) this.Lk.eb()).a(callbackIntent);
        } catch (Throwable e) {
            throw new IllegalStateException(e);
        }
    }

    public void removeLocationUpdates(LocationListener listener) {
        this.Lk.bm();
        er.b((Object) listener, (Object) "Invalid null listener");
        synchronized (this.Ln) {
            com.google.android.gms.location.a aVar = (b) this.Ln.remove(listener);
            if (this.Ll != null && this.Ln.isEmpty()) {
                this.Ll.release();
                this.Ll = null;
            }
            if (aVar != null) {
                aVar.release();
                try {
                    ((hg) this.Lk.eb()).a(aVar);
                } catch (Throwable e) {
                    throw new IllegalStateException(e);
                }
            }
        }
    }

    public void requestLocationUpdates(LocationRequest request, PendingIntent callbackIntent) {
        this.Lk.bm();
        try {
            ((hg) this.Lk.eb()).a(request, callbackIntent);
        } catch (Throwable e) {
            throw new IllegalStateException(e);
        }
    }

    public void requestLocationUpdates(LocationRequest request, LocationListener listener, Looper looper) {
        this.Lk.bm();
        if (looper == null) {
            er.b(Looper.myLooper(), (Object) "Can't create handler inside thread that has not called Looper.prepare()");
        }
        synchronized (this.Ln) {
            com.google.android.gms.location.a bVar;
            b bVar2 = (b) this.Ln.get(listener);
            if (bVar2 == null) {
                bVar = new b(listener, looper);
            } else {
                Object obj = bVar2;
            }
            this.Ln.put(listener, bVar);
            try {
                ((hg) this.Lk.eb()).a(request, bVar, this.mContext.getPackageName());
            } catch (Throwable e) {
                throw new IllegalStateException(e);
            }
        }
    }

    public void setMockLocation(Location mockLocation) {
        this.Lk.bm();
        try {
            ((hg) this.Lk.eb()).setMockLocation(mockLocation);
        } catch (Throwable e) {
            throw new IllegalStateException(e);
        }
    }

    public void setMockMode(boolean isMockMode) {
        this.Lk.bm();
        try {
            ((hg) this.Lk.eb()).setMockMode(isMockMode);
            this.Lm = isMockMode;
        } catch (Throwable e) {
            throw new IllegalStateException(e);
        }
    }
}
