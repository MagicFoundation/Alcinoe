package com.google.android.gms.tagmanager;

import android.content.Context;
import android.net.Uri;
import android.os.Handler;
import android.os.Looper;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class TagManager {
    private static TagManager XB;
    private final DataLayer TN;
    private final r Wj;
    private final ConcurrentMap<n, Boolean> XA;
    private final a Xz;
    private final Context mContext;

    /* renamed from: com.google.android.gms.tagmanager.TagManager.3 */
    static /* synthetic */ class AnonymousClass3 {
        static final /* synthetic */ int[] XD;

        static {
            XD = new int[a.values().length];
            try {
                XD[a.NONE.ordinal()] = 1;
            } catch (NoSuchFieldError e) {
            }
            try {
                XD[a.CONTAINER.ordinal()] = 2;
            } catch (NoSuchFieldError e2) {
            }
            try {
                XD[a.CONTAINER_DEBUG.ordinal()] = 3;
            } catch (NoSuchFieldError e3) {
            }
        }
    }

    interface a {
        o a(Context context, TagManager tagManager, Looper looper, String str, int i, r rVar);
    }

    TagManager(Context context, a containerHolderLoaderProvider, DataLayer dataLayer) {
        if (context == null) {
            throw new NullPointerException("context cannot be null");
        }
        this.mContext = context.getApplicationContext();
        this.Xz = containerHolderLoaderProvider;
        this.XA = new ConcurrentHashMap();
        this.TN = dataLayer;
        this.TN.a(new b() {
            final /* synthetic */ TagManager XC;

            {
                this.XC = r1;
            }

            public void v(Map<String, Object> map) {
                Object obj = map.get(DataLayer.EVENT_KEY);
                if (obj != null) {
                    this.XC.bE(obj.toString());
                }
            }
        });
        this.TN.a(new d(this.mContext));
        this.Wj = new r();
    }

    private void bE(String str) {
        for (n ba : this.XA.keySet()) {
            ba.ba(str);
        }
    }

    public static TagManager getInstance(Context context) {
        TagManager tagManager;
        synchronized (TagManager.class) {
            if (XB == null) {
                if (context == null) {
                    bh.t("TagManager.getInstance requires non-null context.");
                    throw new NullPointerException();
                }
                XB = new TagManager(context, new a() {
                    public o a(Context context, TagManager tagManager, Looper looper, String str, int i, r rVar) {
                        return new o(context, tagManager, looper, str, i, rVar);
                    }
                }, new DataLayer(new v(context)));
            }
            tagManager = XB;
        }
        return tagManager;
    }

    void a(n nVar) {
        this.XA.put(nVar, Boolean.valueOf(true));
    }

    boolean b(n nVar) {
        return this.XA.remove(nVar) != null;
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    synchronized boolean f(Uri uri) {
        boolean z;
        ce ju = ce.ju();
        if (ju.f(uri)) {
            String containerId = ju.getContainerId();
            switch (AnonymousClass3.XD[ju.jv().ordinal()]) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    for (n nVar : this.XA.keySet()) {
                        if (nVar.getContainerId().equals(containerId)) {
                            nVar.bc(null);
                            nVar.refresh();
                        }
                    }
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                case DetectedActivity.STILL /*3*/:
                    for (n nVar2 : this.XA.keySet()) {
                        if (nVar2.getContainerId().equals(containerId)) {
                            nVar2.bc(ju.jw());
                            nVar2.refresh();
                        } else if (nVar2.iF() != null) {
                            nVar2.bc(null);
                            nVar2.refresh();
                        }
                    }
                    break;
            }
        }
        z = false;
        return z;
    }

    public DataLayer getDataLayer() {
        return this.TN;
    }

    public PendingResult<ContainerHolder> loadContainerDefaultOnly(String containerId, int defaultContainerResourceId) {
        PendingResult a = this.Xz.a(this.mContext, this, null, containerId, defaultContainerResourceId, this.Wj);
        a.iI();
        return a;
    }

    public PendingResult<ContainerHolder> loadContainerDefaultOnly(String containerId, int defaultContainerResourceId, Handler handler) {
        PendingResult a = this.Xz.a(this.mContext, this, handler.getLooper(), containerId, defaultContainerResourceId, this.Wj);
        a.iI();
        return a;
    }

    public PendingResult<ContainerHolder> loadContainerPreferFresh(String containerId, int defaultContainerResourceId) {
        PendingResult a = this.Xz.a(this.mContext, this, null, containerId, defaultContainerResourceId, this.Wj);
        a.iK();
        return a;
    }

    public PendingResult<ContainerHolder> loadContainerPreferFresh(String containerId, int defaultContainerResourceId, Handler handler) {
        PendingResult a = this.Xz.a(this.mContext, this, handler.getLooper(), containerId, defaultContainerResourceId, this.Wj);
        a.iK();
        return a;
    }

    public PendingResult<ContainerHolder> loadContainerPreferNonDefault(String containerId, int defaultContainerResourceId) {
        PendingResult a = this.Xz.a(this.mContext, this, null, containerId, defaultContainerResourceId, this.Wj);
        a.iJ();
        return a;
    }

    public PendingResult<ContainerHolder> loadContainerPreferNonDefault(String containerId, int defaultContainerResourceId, Handler handler) {
        PendingResult a = this.Xz.a(this.mContext, this, handler.getLooper(), containerId, defaultContainerResourceId, this.Wj);
        a.iJ();
        return a;
    }

    public void setVerboseLoggingEnabled(boolean enableVerboseLogging) {
        bh.setLogLevel(enableVerboseLogging ? 2 : 5);
    }
}
