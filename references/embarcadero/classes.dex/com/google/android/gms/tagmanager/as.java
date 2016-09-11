package com.google.android.gms.tagmanager;

import android.content.Context;
import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.concurrent.LinkedBlockingQueue;

class as extends Thread implements ar {
    private static as Vc;
    private final LinkedBlockingQueue<Runnable> Vb;
    private volatile at Vd;
    private volatile boolean mClosed;
    private final Context mContext;
    private volatile boolean sa;

    /* renamed from: com.google.android.gms.tagmanager.as.1 */
    class AnonymousClass1 implements Runnable {
        final /* synthetic */ ar Ve;
        final /* synthetic */ long Vf;
        final /* synthetic */ String Vg;
        final /* synthetic */ as Vh;

        AnonymousClass1(as asVar, ar arVar, long j, String str) {
            this.Vh = asVar;
            this.Ve = arVar;
            this.Vf = j;
            this.Vg = str;
        }

        public void run() {
            if (this.Vh.Vd == null) {
                cy kh = cy.kh();
                kh.a(this.Vh.mContext, this.Ve);
                this.Vh.Vd = kh.ki();
            }
            this.Vh.Vd.e(this.Vf, this.Vg);
        }
    }

    private as(Context context) {
        super("GAThread");
        this.Vb = new LinkedBlockingQueue();
        this.sa = false;
        this.mClosed = false;
        if (context != null) {
            this.mContext = context.getApplicationContext();
        } else {
            this.mContext = context;
        }
        start();
    }

    static as H(Context context) {
        if (Vc == null) {
            Vc = new as(context);
        }
        return Vc;
    }

    private String a(Throwable th) {
        OutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        PrintStream printStream = new PrintStream(byteArrayOutputStream);
        th.printStackTrace(printStream);
        printStream.flush();
        return new String(byteArrayOutputStream.toByteArray());
    }

    public void a(Runnable runnable) {
        this.Vb.add(runnable);
    }

    void b(String str, long j) {
        a(new AnonymousClass1(this, this, j, str));
    }

    public void bn(String str) {
        b(str, System.currentTimeMillis());
    }

    public void run() {
        while (!this.mClosed) {
            try {
                Runnable runnable = (Runnable) this.Vb.take();
                if (!this.sa) {
                    runnable.run();
                }
            } catch (InterruptedException e) {
                bh.u(e.toString());
            } catch (Throwable th) {
                bh.t("Error on GAThread: " + a(th));
                bh.t("Google Analytics is shutting down.");
                this.sa = true;
            }
        }
    }
}
