package com.google.android.gms.internal;

import android.content.Context;
import android.util.DisplayMetrics;
import android.view.MotionEvent;
import dalvik.system.DexClassLoader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.ByteBuffer;
import java.util.ArrayList;

public abstract class j extends i {
    private static Method kc;
    private static Method kd;
    private static Method ke;
    private static Method kf;
    private static Method kg;
    private static Method kh;
    private static String ki;
    private static p kj;
    static boolean kk;
    private static long startTime;

    static class a extends Exception {
        public a(Throwable th) {
            super(th);
        }
    }

    static {
        startTime = 0;
        kk = false;
    }

    protected j(Context context, n nVar, o oVar) {
        super(context, nVar, oVar);
    }

    static String a(Context context, n nVar) throws a {
        if (ke == null) {
            throw new a();
        }
        try {
            ByteBuffer byteBuffer = (ByteBuffer) ke.invoke(null, new Object[]{context});
            if (byteBuffer != null) {
                return nVar.a(byteBuffer.array(), true);
            }
            throw new a();
        } catch (Throwable e) {
            throw new a(e);
        } catch (Throwable e2) {
            throw new a(e2);
        }
    }

    static ArrayList<Long> a(MotionEvent motionEvent, DisplayMetrics displayMetrics) throws a {
        if (kf == null || motionEvent == null) {
            throw new a();
        }
        try {
            return (ArrayList) kf.invoke(null, new Object[]{motionEvent, displayMetrics});
        } catch (Throwable e) {
            throw new a(e);
        } catch (Throwable e2) {
            throw new a(e2);
        }
    }

    protected static synchronized void a(String str, Context context, n nVar) {
        synchronized (j.class) {
            if (!kk) {
                try {
                    kj = new p(nVar, null);
                    ki = str;
                    e(context);
                    startTime = w().longValue();
                    kk = true;
                } catch (a e) {
                } catch (UnsupportedOperationException e2) {
                }
            }
        }
    }

    static String b(Context context, n nVar) throws a {
        if (kh == null) {
            throw new a();
        }
        try {
            ByteBuffer byteBuffer = (ByteBuffer) kh.invoke(null, new Object[]{context});
            if (byteBuffer != null) {
                return nVar.a(byteBuffer.array(), true);
            }
            throw new a();
        } catch (Throwable e) {
            throw new a(e);
        } catch (Throwable e2) {
            throw new a(e2);
        }
    }

    private static String b(byte[] bArr, String str) throws a {
        try {
            return new String(kj.c(bArr, str), "UTF-8");
        } catch (Throwable e) {
            throw new a(e);
        } catch (Throwable e2) {
            throw new a(e2);
        }
    }

    static String d(Context context) throws a {
        if (kg == null) {
            throw new a();
        }
        try {
            String str = (String) kg.invoke(null, new Object[]{context});
            if (str != null) {
                return str;
            }
            throw new a();
        } catch (Throwable e) {
            throw new a(e);
        } catch (Throwable e2) {
            throw new a(e2);
        }
    }

    private static void e(Context context) throws a {
        try {
            byte[] d = kj.d(r.getKey());
            byte[] c = kj.c(d, r.A());
            File cacheDir = context.getCacheDir();
            if (cacheDir == null) {
                cacheDir = context.getDir("dex", 0);
                if (cacheDir == null) {
                    throw new a();
                }
            }
            File createTempFile = File.createTempFile("ads", ".jar", cacheDir);
            FileOutputStream fileOutputStream = new FileOutputStream(createTempFile);
            fileOutputStream.write(c, 0, c.length);
            fileOutputStream.close();
            DexClassLoader dexClassLoader = new DexClassLoader(createTempFile.getAbsolutePath(), cacheDir.getAbsolutePath(), null, context.getClassLoader());
            Class loadClass = dexClassLoader.loadClass(b(d, r.B()));
            Class loadClass2 = dexClassLoader.loadClass(b(d, r.H()));
            Class loadClass3 = dexClassLoader.loadClass(b(d, r.F()));
            Class loadClass4 = dexClassLoader.loadClass(b(d, r.L()));
            Class loadClass5 = dexClassLoader.loadClass(b(d, r.D()));
            Class loadClass6 = dexClassLoader.loadClass(b(d, r.J()));
            kc = loadClass.getMethod(b(d, r.C()), new Class[0]);
            kd = loadClass2.getMethod(b(d, r.I()), new Class[0]);
            ke = loadClass3.getMethod(b(d, r.G()), new Class[]{Context.class});
            kf = loadClass4.getMethod(b(d, r.M()), new Class[]{MotionEvent.class, DisplayMetrics.class});
            kg = loadClass5.getMethod(b(d, r.E()), new Class[]{Context.class});
            kh = loadClass6.getMethod(b(d, r.K()), new Class[]{Context.class});
            String name = createTempFile.getName();
            createTempFile.delete();
            new File(cacheDir, name.replace(".jar", ".dex")).delete();
        } catch (Throwable e) {
            throw new a(e);
        } catch (Throwable e2) {
            throw new a(e2);
        } catch (Throwable e22) {
            throw new a(e22);
        } catch (Throwable e222) {
            throw new a(e222);
        } catch (Throwable e2222) {
            throw new a(e2222);
        } catch (Throwable e22222) {
            throw new a(e22222);
        }
    }

    static String v() throws a {
        if (ki != null) {
            return ki;
        }
        throw new a();
    }

    static Long w() throws a {
        if (kc == null) {
            throw new a();
        }
        try {
            return (Long) kc.invoke(null, new Object[0]);
        } catch (Throwable e) {
            throw new a(e);
        } catch (Throwable e2) {
            throw new a(e2);
        }
    }

    static String x() throws a {
        if (kd == null) {
            throw new a();
        }
        try {
            return (String) kd.invoke(null, new Object[0]);
        } catch (Throwable e) {
            throw new a(e);
        } catch (Throwable e2) {
            throw new a(e2);
        }
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    protected void b(Context context) {
        try {
            a(1, x());
        } catch (a e) {
        } catch (IOException e2) {
            return;
        }
        a(2, v());
        a(25, w().longValue());
        a(24, d(context));
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    protected void c(Context context) {
        try {
            a(2, v());
        } catch (a e) {
        } catch (IOException e2) {
            return;
        }
        try {
            a(1, x());
        } catch (a e3) {
        } catch (IOException e22) {
            return;
        }
        try {
            long longValue = w().longValue();
            a(25, longValue);
            if (startTime != 0) {
                a(17, longValue - startTime);
                a(23, startTime);
            }
        } catch (a e4) {
        } catch (IOException e222) {
            return;
        }
        ArrayList a = a(this.jY, this.jZ);
        a(14, ((Long) a.get(0)).longValue());
        a(15, ((Long) a.get(1)).longValue());
        if (a.size() >= 3) {
            a(16, ((Long) a.get(2)).longValue());
        }
        a(27, a(context, this.ka));
        a(29, b(context, this.ka));
    }
}
