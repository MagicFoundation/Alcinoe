package com.google.android.gms.common.images;

import android.app.ActivityManager;
import android.content.ComponentCallbacks2;
import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.os.ParcelFileDescriptor;
import android.os.ResultReceiver;
import android.support.v4.view.accessibility.AccessibilityEventCompat;
import android.util.Log;
import android.widget.ImageView;
import com.google.android.gms.internal.ed;
import com.google.android.gms.internal.ev;
import com.google.android.gms.internal.fr;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public final class ImageManager {
    private static final Object Ar;
    private static HashSet<Uri> As;
    private static ImageManager At;
    private static ImageManager Au;
    private final ExecutorService Av;
    private final b Aw;
    private final Map<a, ImageReceiver> Ax;
    private final Map<Uri, ImageReceiver> Ay;
    private final Context mContext;
    private final Handler mHandler;

    private final class ImageReceiver extends ResultReceiver {
        boolean AA;
        final /* synthetic */ ImageManager AB;
        private final ArrayList<a> Az;
        private final Uri mUri;

        ImageReceiver(ImageManager imageManager, Uri uri) {
            this.AB = imageManager;
            super(new Handler(Looper.getMainLooper()));
            this.AA = false;
            this.mUri = uri;
            this.Az = new ArrayList();
        }

        public void c(a aVar) {
            ed.a(!this.AA, "Cannot add an ImageRequest when mHandlingRequests is true");
            ed.ac("ImageReceiver.addImageRequest() must be called in the main thread");
            this.Az.add(aVar);
        }

        public void d(a aVar) {
            ed.a(!this.AA, "Cannot remove an ImageRequest when mHandlingRequests is true");
            ed.ac("ImageReceiver.removeImageRequest() must be called in the main thread");
            this.Az.remove(aVar);
        }

        public void dN() {
            Intent intent = new Intent("com.google.android.gms.common.images.LOAD_IMAGE");
            intent.putExtra("com.google.android.gms.extras.uri", this.mUri);
            intent.putExtra("com.google.android.gms.extras.resultReceiver", this);
            intent.putExtra("com.google.android.gms.extras.priority", 3);
            this.AB.mContext.sendBroadcast(intent);
        }

        public void onReceiveResult(int resultCode, Bundle resultData) {
            this.AB.Av.execute(new c(this.AB, this.mUri, (ParcelFileDescriptor) resultData.getParcelable("com.google.android.gms.extra.fileDescriptor")));
        }
    }

    public interface OnImageLoadedListener {
        void onImageLoaded(Uri uri, Drawable drawable, boolean z);
    }

    private static final class a {
        static int a(ActivityManager activityManager) {
            return activityManager.getLargeMemoryClass();
        }
    }

    private final class c implements Runnable {
        final /* synthetic */ ImageManager AB;
        private final ParcelFileDescriptor AC;
        private final Uri mUri;

        public c(ImageManager imageManager, Uri uri, ParcelFileDescriptor parcelFileDescriptor) {
            this.AB = imageManager;
            this.mUri = uri;
            this.AC = parcelFileDescriptor;
        }

        public void run() {
            ed.ad("LoadBitmapFromDiskRunnable can't be executed in the main thread");
            boolean z = false;
            Bitmap bitmap = null;
            if (this.AC != null) {
                try {
                    bitmap = BitmapFactory.decodeFileDescriptor(this.AC.getFileDescriptor());
                } catch (Throwable e) {
                    Log.e("ImageManager", "OOM while loading bitmap for uri: " + this.mUri, e);
                    z = true;
                }
                try {
                    this.AC.close();
                } catch (Throwable e2) {
                    Log.e("ImageManager", "closed failed", e2);
                }
            }
            CountDownLatch countDownLatch = new CountDownLatch(1);
            this.AB.mHandler.post(new f(this.AB, this.mUri, bitmap, z, countDownLatch));
            try {
                countDownLatch.await();
            } catch (InterruptedException e3) {
                Log.w("ImageManager", "Latch interrupted while posting " + this.mUri);
            }
        }
    }

    private final class d implements Runnable {
        final /* synthetic */ ImageManager AB;
        private final a AD;

        public d(ImageManager imageManager, a aVar) {
            this.AB = imageManager;
            this.AD = aVar;
        }

        public void run() {
            ed.ac("LoadImageRunnable must be executed on the main thread");
            this.AB.b(this.AD);
            com.google.android.gms.common.images.a.a aVar = this.AD.AF;
            if (aVar.uri == null) {
                this.AD.b(this.AB.mContext, true);
                return;
            }
            Bitmap a = this.AB.a(aVar);
            if (a != null) {
                this.AD.a(this.AB.mContext, a, true);
                return;
            }
            this.AD.x(this.AB.mContext);
            ImageReceiver imageReceiver = (ImageReceiver) this.AB.Ay.get(aVar.uri);
            if (imageReceiver == null) {
                imageReceiver = new ImageReceiver(this.AB, aVar.uri);
                this.AB.Ay.put(aVar.uri, imageReceiver);
            }
            imageReceiver.c(this.AD);
            if (this.AD.AI != 1) {
                this.AB.Ax.put(this.AD, imageReceiver);
            }
            synchronized (ImageManager.Ar) {
                if (!ImageManager.As.contains(aVar.uri)) {
                    ImageManager.As.add(aVar.uri);
                    imageReceiver.dN();
                }
            }
        }
    }

    private static final class e implements ComponentCallbacks2 {
        private final b Aw;

        public e(b bVar) {
            this.Aw = bVar;
        }

        public void onConfigurationChanged(Configuration newConfig) {
        }

        public void onLowMemory() {
            this.Aw.evictAll();
        }

        public void onTrimMemory(int level) {
            if (level >= 60) {
                this.Aw.evictAll();
            } else if (level >= 20) {
                this.Aw.trimToSize(this.Aw.size() / 2);
            }
        }
    }

    private final class f implements Runnable {
        final /* synthetic */ ImageManager AB;
        private boolean AE;
        private final Bitmap mBitmap;
        private final Uri mUri;
        private final CountDownLatch zf;

        public f(ImageManager imageManager, Uri uri, Bitmap bitmap, boolean z, CountDownLatch countDownLatch) {
            this.AB = imageManager;
            this.mUri = uri;
            this.mBitmap = bitmap;
            this.AE = z;
            this.zf = countDownLatch;
        }

        private void a(ImageReceiver imageReceiver, boolean z) {
            imageReceiver.AA = true;
            ArrayList a = imageReceiver.Az;
            int size = a.size();
            for (int i = 0; i < size; i++) {
                a aVar = (a) a.get(i);
                if (z) {
                    aVar.a(this.AB.mContext, this.mBitmap, false);
                } else {
                    aVar.b(this.AB.mContext, false);
                }
                if (aVar.AI != 1) {
                    this.AB.Ax.remove(aVar);
                }
            }
            imageReceiver.AA = false;
        }

        public void run() {
            ed.ac("OnBitmapLoadedRunnable must be executed in the main thread");
            boolean z = this.mBitmap != null;
            if (this.AB.Aw != null) {
                if (this.AE) {
                    this.AB.Aw.evictAll();
                    System.gc();
                    this.AE = false;
                    this.AB.mHandler.post(this);
                    return;
                } else if (z) {
                    this.AB.Aw.put(new com.google.android.gms.common.images.a.a(this.mUri), this.mBitmap);
                }
            }
            ImageReceiver imageReceiver = (ImageReceiver) this.AB.Ay.remove(this.mUri);
            if (imageReceiver != null) {
                a(imageReceiver, z);
            }
            this.zf.countDown();
            synchronized (ImageManager.Ar) {
                ImageManager.As.remove(this.mUri);
            }
        }
    }

    private static final class b extends ev<com.google.android.gms.common.images.a.a, Bitmap> {
        public b(Context context) {
            super(w(context));
        }

        private static int w(Context context) {
            ActivityManager activityManager = (ActivityManager) context.getSystemService("activity");
            int memoryClass = (((context.getApplicationInfo().flags & AccessibilityEventCompat.TYPE_TOUCH_INTERACTION_START) != 0 ? 1 : null) == null || !fr.eJ()) ? activityManager.getMemoryClass() : a.a(activityManager);
            return (int) (((float) (memoryClass * AccessibilityEventCompat.TYPE_TOUCH_INTERACTION_START)) * 0.33f);
        }

        protected int a(com.google.android.gms.common.images.a.a aVar, Bitmap bitmap) {
            return bitmap.getHeight() * bitmap.getRowBytes();
        }

        protected void a(boolean z, com.google.android.gms.common.images.a.a aVar, Bitmap bitmap, Bitmap bitmap2) {
            super.entryRemoved(z, aVar, bitmap, bitmap2);
        }

        protected /* synthetic */ void entryRemoved(boolean x0, Object x1, Object x2, Object x3) {
            a(x0, (com.google.android.gms.common.images.a.a) x1, (Bitmap) x2, (Bitmap) x3);
        }

        protected /* synthetic */ int sizeOf(Object x0, Object x1) {
            return a((com.google.android.gms.common.images.a.a) x0, (Bitmap) x1);
        }
    }

    static {
        Ar = new Object();
        As = new HashSet();
    }

    private ImageManager(Context context, boolean withMemoryCache) {
        this.mContext = context.getApplicationContext();
        this.mHandler = new Handler(Looper.getMainLooper());
        this.Av = Executors.newFixedThreadPool(4);
        if (withMemoryCache) {
            this.Aw = new b(this.mContext);
            if (fr.eM()) {
                dL();
            }
        } else {
            this.Aw = null;
        }
        this.Ax = new HashMap();
        this.Ay = new HashMap();
    }

    private Bitmap a(com.google.android.gms.common.images.a.a aVar) {
        return this.Aw == null ? null : (Bitmap) this.Aw.get(aVar);
    }

    public static ImageManager a(Context context, boolean z) {
        if (z) {
            if (Au == null) {
                Au = new ImageManager(context, true);
            }
            return Au;
        }
        if (At == null) {
            At = new ImageManager(context, false);
        }
        return At;
    }

    private boolean b(a aVar) {
        ed.ac("ImageManager.cleanupHashMaps() must be called in the main thread");
        if (aVar.AI == 1) {
            return true;
        }
        ImageReceiver imageReceiver = (ImageReceiver) this.Ax.get(aVar);
        if (imageReceiver == null) {
            return true;
        }
        if (imageReceiver.AA) {
            return false;
        }
        this.Ax.remove(aVar);
        imageReceiver.d(aVar);
        return true;
    }

    public static ImageManager create(Context context) {
        return a(context, false);
    }

    private void dL() {
        this.mContext.registerComponentCallbacks(new e(this.Aw));
    }

    public void a(a aVar) {
        ed.ac("ImageManager.loadImage() must be called in the main thread");
        boolean b = b(aVar);
        Runnable dVar = new d(this, aVar);
        if (b) {
            dVar.run();
        } else {
            this.mHandler.post(dVar);
        }
    }

    public void loadImage(ImageView imageView, int resId) {
        a aVar = new a(resId);
        aVar.a(imageView);
        a(aVar);
    }

    public void loadImage(ImageView imageView, Uri uri) {
        a aVar = new a(uri);
        aVar.a(imageView);
        a(aVar);
    }

    public void loadImage(ImageView imageView, Uri uri, int defaultResId) {
        a aVar = new a(uri);
        aVar.L(defaultResId);
        aVar.a(imageView);
        a(aVar);
    }

    public void loadImage(OnImageLoadedListener listener, Uri uri) {
        a aVar = new a(uri);
        aVar.a(listener);
        a(aVar);
    }

    public void loadImage(OnImageLoadedListener listener, Uri uri, int defaultResId) {
        a aVar = new a(uri);
        aVar.L(defaultResId);
        aVar.a(listener);
        a(aVar);
    }
}
