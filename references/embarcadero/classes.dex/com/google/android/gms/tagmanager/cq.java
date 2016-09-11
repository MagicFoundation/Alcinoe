package com.google.android.gms.tagmanager;

import android.content.Context;
import android.content.res.Resources.NotFoundException;
import com.google.android.gms.internal.c.f;
import com.google.android.gms.internal.jd.a;
import com.google.android.gms.internal.kd;
import com.google.android.gms.internal.ke;
import com.google.android.gms.tagmanager.cr.c;
import com.google.android.gms.tagmanager.cr.g;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.json.JSONException;

class cq implements f {
    private final String TM;
    private bg<a> Wi;
    private final ExecutorService Wp;
    private final Context mContext;

    /* renamed from: com.google.android.gms.tagmanager.cq.2 */
    class AnonymousClass2 implements Runnable {
        final /* synthetic */ cq Wq;
        final /* synthetic */ a Wr;

        AnonymousClass2(cq cqVar, a aVar) {
            this.Wq = cqVar;
            this.Wr = aVar;
        }

        public void run() {
            this.Wq.c(this.Wr);
        }
    }

    cq(Context context, String str) {
        this.mContext = context;
        this.TM = str;
        this.Wp = Executors.newSingleThreadExecutor();
    }

    private c a(ByteArrayOutputStream byteArrayOutputStream) {
        c cVar = null;
        try {
            cVar = ba.br(byteArrayOutputStream.toString("UTF-8"));
        } catch (UnsupportedEncodingException e) {
            bh.s("Tried to convert binary resource to string for JSON parsing; not UTF-8 format");
        } catch (JSONException e2) {
            bh.w("Resource is a UTF-8 encoded string but doesn't contain a JSON container");
        }
        return cVar;
    }

    private c k(byte[] bArr) {
        c cVar = null;
        try {
            cVar = cr.b(f.a(bArr));
        } catch (kd e) {
            bh.w("Resource doesn't contain a binary container");
        } catch (g e2) {
            bh.w("Resource doesn't contain a binary container");
        }
        return cVar;
    }

    public void a(bg<a> bgVar) {
        this.Wi = bgVar;
    }

    public void b(a aVar) {
        this.Wp.execute(new AnonymousClass2(this, aVar));
    }

    public c bP(int i) {
        bh.v("Atttempting to load container from resource ID " + i);
        try {
            InputStream openRawResource = this.mContext.getResources().openRawResource(i);
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            cr.b(openRawResource, byteArrayOutputStream);
            c a = a(byteArrayOutputStream);
            return a != null ? a : k(byteArrayOutputStream.toByteArray());
        } catch (IOException e) {
            bh.w("Error reading default container resource with ID " + i);
            return null;
        } catch (NotFoundException e2) {
            bh.w("No default container resource found.");
            return null;
        }
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    boolean c(a aVar) {
        boolean z = false;
        File jD = jD();
        try {
            FileOutputStream fileOutputStream = new FileOutputStream(jD);
        } catch (FileNotFoundException e) {
            bh.t("Error opening resource file for writing");
        }
        try {
            fileOutputStream.write(ke.d(aVar));
            z = true;
            try {
                fileOutputStream.close();
            } catch (IOException e2) {
                bh.w("error closing stream for writing resource to disk");
            }
        } catch (IOException e3) {
            bh.w("Error writing resource to disk. Removing resource from disk.");
            jD.delete();
        } catch (Throwable th) {
            try {
                fileOutputStream.close();
            } catch (IOException e4) {
                bh.w("error closing stream for writing resource to disk");
            }
        }
        return z;
    }

    public void iN() {
        this.Wp.execute(new Runnable() {
            final /* synthetic */ cq Wq;

            {
                this.Wq = r1;
            }

            public void run() {
                this.Wq.jC();
            }
        });
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    void jC() {
        if (this.Wi == null) {
            throw new IllegalStateException("callback must be set before execute");
        }
        this.Wi.iM();
        bh.v("Start loading resource from disk ...");
        if ((ce.ju().jv() == a.CONTAINER || ce.ju().jv() == a.CONTAINER_DEBUG) && this.TM.equals(ce.ju().getContainerId())) {
            this.Wi.a(bg.a.NOT_AVAILABLE);
            return;
        }
        try {
            InputStream fileInputStream = new FileInputStream(jD());
            try {
                OutputStream byteArrayOutputStream = new ByteArrayOutputStream();
                cr.b(fileInputStream, byteArrayOutputStream);
                this.Wi.i(a.l(byteArrayOutputStream.toByteArray()));
                try {
                    fileInputStream.close();
                } catch (IOException e) {
                    bh.w("error closing stream for reading resource from disk");
                }
            } catch (IOException e2) {
                bh.w("error reading resource from disk");
                this.Wi.a(bg.a.IO_ERROR);
            } catch (Throwable th) {
                try {
                    fileInputStream.close();
                } catch (IOException e3) {
                    bh.w("error closing stream for reading resource from disk");
                }
            }
            bh.v("Load resource from disk finished.");
        } catch (FileNotFoundException e4) {
            bh.s("resource not on disk");
            this.Wi.a(bg.a.NOT_AVAILABLE);
        }
    }

    File jD() {
        return new File(this.mContext.getDir("google_tagmanager", 0), "resource_" + this.TM);
    }

    public synchronized void release() {
        this.Wp.shutdown();
    }
}
