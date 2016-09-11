package com.google.android.gms.analytics;

import android.content.Context;
import android.os.Process;
import android.support.v4.os.EnvironmentCompat;
import android.support.v4.view.accessibility.AccessibilityNodeInfoCompat;
import android.text.TextUtils;
import com.google.analytics.tracking.android.ModelFields;
import com.google.android.gms.analytics.internal.Command;
import com.google.android.gms.internal.di;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;

class t extends Thread implements f {
    private static t sd;
    private volatile boolean mClosed;
    private final Context mContext;
    private volatile String qX;
    private final LinkedBlockingQueue<Runnable> rZ;
    private volatile boolean sa;
    private volatile List<di> sb;
    private volatile String sc;
    private volatile ag se;

    /* renamed from: com.google.android.gms.analytics.t.1 */
    class AnonymousClass1 implements Runnable {
        final /* synthetic */ Map sf;
        final /* synthetic */ t sg;

        AnonymousClass1(t tVar, Map map) {
            this.sg = tVar;
            this.sf = map;
        }

        public void run() {
            if (TextUtils.isEmpty((CharSequence) this.sf.get("&cid"))) {
                this.sf.put("&cid", this.sg.qX);
            }
            if (!GoogleAnalytics.getInstance(this.sg.mContext).getAppOptOut() && !this.sg.p(this.sf)) {
                if (!TextUtils.isEmpty(this.sg.sc)) {
                    u.bR().r(true);
                    this.sf.putAll(new HitBuilder().setCampaignParamsFromUrl(this.sg.sc).build());
                    u.bR().r(false);
                    this.sg.sc = null;
                }
                this.sg.r(this.sf);
                this.sg.q(this.sf);
                this.sg.se.b(y.s(this.sf), Long.valueOf((String) this.sf.get("&ht")).longValue(), this.sg.o(this.sf), this.sg.sb);
            }
        }
    }

    private t(Context context) {
        super("GAThread");
        this.rZ = new LinkedBlockingQueue();
        this.sa = false;
        this.mClosed = false;
        if (context != null) {
            this.mContext = context.getApplicationContext();
        } else {
            this.mContext = context;
        }
        start();
    }

    static int C(String str) {
        int i = 1;
        if (!TextUtils.isEmpty(str)) {
            i = 0;
            for (int length = str.length() - 1; length >= 0; length--) {
                char charAt = str.charAt(length);
                i = (((i << 6) & 268435455) + charAt) + (charAt << 14);
                int i2 = 266338304 & i;
                if (i2 != 0) {
                    i ^= i2 >> 21;
                }
            }
        }
        return i;
    }

    private String a(Throwable th) {
        OutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        PrintStream printStream = new PrintStream(byteArrayOutputStream);
        th.printStackTrace(printStream);
        printStream.flush();
        return new String(byteArrayOutputStream.toByteArray());
    }

    private String o(Map<String, String> map) {
        return map.containsKey(ModelFields.USE_SECURE) ? ak.d((String) map.get(ModelFields.USE_SECURE), true) ? "https:" : "http:" : "https:";
    }

    private boolean p(Map<String, String> map) {
        if (map.get("&sf") == null) {
            return false;
        }
        double a = ak.a((String) map.get("&sf"), 100.0d);
        if (a >= 100.0d) {
            return false;
        }
        if (((double) (C((String) map.get("&cid")) % 10000)) < a * 100.0d) {
            return false;
        }
        String str = map.get("&t") == null ? EnvironmentCompat.MEDIA_UNKNOWN : (String) map.get("&t");
        aa.v(String.format("%s hit sampled out", new Object[]{str}));
        return true;
    }

    static t q(Context context) {
        if (sd == null) {
            sd = new t(context);
        }
        return sd;
    }

    private void q(Map<String, String> map) {
        m m = a.m(this.mContext);
        ak.a(map, "&adid", m.getValue("&adid"));
        ak.a(map, "&ate", m.getValue("&ate"));
    }

    static String r(Context context) {
        try {
            FileInputStream openFileInput = context.openFileInput("gaInstallData");
            byte[] bArr = new byte[AccessibilityNodeInfoCompat.ACTION_SCROLL_BACKWARD];
            int read = openFileInput.read(bArr, 0, AccessibilityNodeInfoCompat.ACTION_SCROLL_BACKWARD);
            if (openFileInput.available() > 0) {
                aa.t("Too much campaign data, ignoring it.");
                openFileInput.close();
                context.deleteFile("gaInstallData");
                return null;
            }
            openFileInput.close();
            context.deleteFile("gaInstallData");
            if (read <= 0) {
                aa.w("Campaign file is empty.");
                return null;
            }
            String str = new String(bArr, 0, read);
            aa.u("Campaign found: " + str);
            return str;
        } catch (FileNotFoundException e) {
            aa.u("No campaign data found.");
            return null;
        } catch (IOException e2) {
            aa.t("Error reading campaign data.");
            context.deleteFile("gaInstallData");
            return null;
        }
    }

    private void r(Map<String, String> map) {
        m bt = g.bt();
        ak.a(map, "&an", bt.getValue("&an"));
        ak.a(map, "&av", bt.getValue("&av"));
        ak.a(map, "&aid", bt.getValue("&aid"));
        ak.a(map, "&aiid", bt.getValue("&aiid"));
        map.put("&v", "1");
    }

    void a(Runnable runnable) {
        this.rZ.add(runnable);
    }

    public void bk() {
        a(new Runnable() {
            final /* synthetic */ t sg;

            {
                this.sg = r1;
            }

            public void run() {
                this.sg.se.bk();
            }
        });
    }

    public void bp() {
        a(new Runnable() {
            final /* synthetic */ t sg;

            {
                this.sg = r1;
            }

            public void run() {
                this.sg.se.bp();
            }
        });
    }

    public void br() {
        a(new Runnable() {
            final /* synthetic */ t sg;

            {
                this.sg = r1;
            }

            public void run() {
                this.sg.se.br();
            }
        });
    }

    public LinkedBlockingQueue<Runnable> bs() {
        return this.rZ;
    }

    public Thread getThread() {
        return this;
    }

    protected void init() {
        this.se.bI();
        this.sb = new ArrayList();
        this.sb.add(new di(Command.APPEND_VERSION, "&_v".substring(1), "ma4.0.0"));
        this.sb.add(new di(Command.APPEND_QUEUE_TIME, "&qt".substring(1), null));
        this.sb.add(new di(Command.APPEND_CACHE_BUSTER, "&z".substring(1), null));
    }

    public void n(Map<String, String> map) {
        Map hashMap = new HashMap(map);
        String str = (String) map.get("&ht");
        if (str != null) {
            try {
                Long.valueOf(str);
            } catch (NumberFormatException e) {
                str = null;
            }
        }
        if (str == null) {
            hashMap.put("&ht", Long.toString(System.currentTimeMillis()));
        }
        a(new AnonymousClass1(this, hashMap));
    }

    public void run() {
        Process.setThreadPriority(10);
        try {
            Thread.sleep(5000);
        } catch (InterruptedException e) {
            aa.w("sleep interrupted in GAThread initialize");
        }
        try {
            if (this.se == null) {
                this.se = new s(this.mContext, this);
            }
            init();
            this.qX = h.bu().getValue("&cid");
            if (this.qX == null) {
                this.sa = true;
            }
            this.sc = r(this.mContext);
            aa.v("Initialized GA Thread");
        } catch (Throwable th) {
            aa.t("Error initializing the GAThread: " + a(th));
            aa.t("Google Analytics will not start up.");
            this.sa = true;
        }
        while (!this.mClosed) {
            try {
                Runnable runnable = (Runnable) this.rZ.take();
                if (!this.sa) {
                    runnable.run();
                }
            } catch (InterruptedException e2) {
                aa.u(e2.toString());
            } catch (Throwable th2) {
                aa.t("Error on GAThread: " + a(th2));
                aa.t("Google Analytics is shutting down.");
                this.sa = true;
            }
        }
    }
}
