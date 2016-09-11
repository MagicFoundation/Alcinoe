package com.google.android.gms.tagmanager;

import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import com.google.android.gms.internal.c.j;
import com.google.android.gms.tagmanager.bg.a;
import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.OutputStream;

class co implements Runnable {
    private final String TM;
    private volatile String Ui;
    private final bm Wg;
    private final String Wh;
    private bg<j> Wi;
    private volatile r Wj;
    private volatile String Wk;
    private final Context mContext;

    co(Context context, String str, bm bmVar, r rVar) {
        this.mContext = context;
        this.Wg = bmVar;
        this.TM = str;
        this.Wj = rVar;
        this.Wh = "/r?id=" + str;
        this.Ui = this.Wh;
        this.Wk = null;
    }

    public co(Context context, String str, r rVar) {
        this(context, str, new bm(), rVar);
    }

    private boolean jx() {
        NetworkInfo activeNetworkInfo = ((ConnectivityManager) this.mContext.getSystemService("connectivity")).getActiveNetworkInfo();
        if (activeNetworkInfo != null && activeNetworkInfo.isConnected()) {
            return true;
        }
        bh.v("...no network connectivity");
        return false;
    }

    private void jy() {
        if (jx()) {
            bh.v("Start loading resource from network ...");
            String jz = jz();
            bl ji = this.Wg.ji();
            try {
                InputStream bo = ji.bo(jz);
                try {
                    OutputStream byteArrayOutputStream = new ByteArrayOutputStream();
                    cr.b(bo, byteArrayOutputStream);
                    j b = j.b(byteArrayOutputStream.toByteArray());
                    bh.v("Successfully loaded supplemented resource: " + b);
                    if (b.fV == null && b.fU.length == 0) {
                        bh.v("No change for container: " + this.TM);
                    }
                    this.Wi.i(b);
                    bh.v("Load resource from network finished.");
                } catch (Throwable e) {
                    bh.b("Error when parsing downloaded resources from url: " + jz + " " + e.getMessage(), e);
                    this.Wi.a(a.SERVER_ERROR);
                    ji.close();
                }
            } catch (FileNotFoundException e2) {
                bh.w("No data is retrieved from the given url: " + jz + ". Make sure container_id: " + this.TM + " is correct.");
                this.Wi.a(a.SERVER_ERROR);
            } catch (Throwable e3) {
                bh.b("Error when loading resources from url: " + jz + " " + e3.getMessage(), e3);
                this.Wi.a(a.IO_ERROR);
            } finally {
                ji.close();
            }
        } else {
            this.Wi.a(a.NOT_AVAILABLE);
        }
    }

    void a(bg<j> bgVar) {
        this.Wi = bgVar;
    }

    void bf(String str) {
        if (str == null) {
            this.Ui = this.Wh;
            return;
        }
        bh.s("Setting CTFE URL path: " + str);
        this.Ui = str;
    }

    void bu(String str) {
        bh.s("Setting previous container version: " + str);
        this.Wk = str;
    }

    String jz() {
        String str = this.Wj.iO() + this.Ui + "&v=a59512756";
        if (!(this.Wk == null || this.Wk.trim().equals(""))) {
            str = str + "&pv=" + this.Wk;
        }
        return ce.ju().jv().equals(a.CONTAINER_DEBUG) ? str + "&gtm_debug=x" : str;
    }

    public void run() {
        if (this.Wi == null) {
            throw new IllegalStateException("callback must be set before execute");
        }
        this.Wi.iM();
        jy();
    }
}
