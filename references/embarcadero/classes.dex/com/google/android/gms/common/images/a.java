package com.google.android.gms.common.images;

import android.content.Context;
import android.content.res.Resources;
import android.graphics.Bitmap;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.widget.ImageView;
import android.widget.TextView;
import com.google.android.gms.common.images.ImageManager.OnImageLoadedListener;
import com.google.android.gms.internal.ea;
import com.google.android.gms.internal.eb;
import com.google.android.gms.internal.ec;
import com.google.android.gms.internal.ed;
import com.google.android.gms.internal.ep;
import com.google.android.gms.internal.fr;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.lang.ref.WeakReference;

public final class a {
    final a AF;
    private int AG;
    private int AH;
    int AI;
    private int AJ;
    private WeakReference<OnImageLoadedListener> AK;
    private WeakReference<ImageView> AL;
    private WeakReference<TextView> AM;
    private int AN;
    private boolean AO;
    private boolean AP;
    private int AQ;

    public static final class a {
        public final Uri uri;

        public a(Uri uri) {
            this.uri = uri;
        }

        public boolean equals(Object obj) {
            if (obj instanceof a) {
                return this == obj || ((a) obj).hashCode() == hashCode();
            } else {
                return false;
            }
        }

        public int hashCode() {
            return ep.hashCode(this.uri);
        }
    }

    public a(int i) {
        this.AG = 0;
        this.AH = 0;
        this.AN = -1;
        this.AO = true;
        this.AP = false;
        this.AF = new a(null);
        this.AH = i;
    }

    public a(Uri uri) {
        this.AG = 0;
        this.AH = 0;
        this.AN = -1;
        this.AO = true;
        this.AP = false;
        this.AF = new a(uri);
        this.AH = 0;
    }

    private ea a(Drawable drawable, Drawable drawable2) {
        if (drawable == null) {
            drawable = null;
        } else if (drawable instanceof ea) {
            drawable = ((ea) drawable).dO();
        }
        return new ea(drawable, drawable2);
    }

    private void a(Drawable drawable, boolean z, boolean z2, boolean z3) {
        switch (this.AI) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                if (!z2) {
                    OnImageLoadedListener onImageLoadedListener = (OnImageLoadedListener) this.AK.get();
                    if (onImageLoadedListener != null) {
                        onImageLoadedListener.onImageLoaded(this.AF.uri, drawable, z3);
                    }
                }
            case DetectedActivity.ON_FOOT /*2*/:
                ImageView imageView = (ImageView) this.AL.get();
                if (imageView != null) {
                    a(imageView, drawable, z, z2, z3);
                }
            case DetectedActivity.STILL /*3*/:
                TextView textView = (TextView) this.AM.get();
                if (textView != null) {
                    a(textView, this.AN, drawable, z, z2);
                }
            default:
        }
    }

    private void a(ImageView imageView, Drawable drawable, boolean z, boolean z2, boolean z3) {
        Object obj = (z2 || z3) ? null : 1;
        if (obj != null && (imageView instanceof ec)) {
            int dQ = ((ec) imageView).dQ();
            if (this.AH != 0 && dQ == this.AH) {
                return;
            }
        }
        boolean b = b(z, z2);
        Drawable a = b ? a(imageView.getDrawable(), drawable) : drawable;
        imageView.setImageDrawable(a);
        if (imageView instanceof ec) {
            ec ecVar = (ec) imageView;
            ecVar.d(z3 ? this.AF.uri : null);
            ecVar.N(obj != null ? this.AH : 0);
        }
        if (b) {
            ((ea) a).startTransition(250);
        }
    }

    private void a(TextView textView, int i, Drawable drawable, boolean z, boolean z2) {
        boolean b = b(z, z2);
        Drawable[] compoundDrawablesRelative = fr.eO() ? textView.getCompoundDrawablesRelative() : textView.getCompoundDrawables();
        Drawable a = b ? a(compoundDrawablesRelative[i], drawable) : drawable;
        Drawable drawable2 = i == 0 ? a : compoundDrawablesRelative[0];
        Drawable drawable3 = i == 1 ? a : compoundDrawablesRelative[1];
        Drawable drawable4 = i == 2 ? a : compoundDrawablesRelative[2];
        Drawable drawable5 = i == 3 ? a : compoundDrawablesRelative[3];
        if (fr.eO()) {
            textView.setCompoundDrawablesRelativeWithIntrinsicBounds(drawable2, drawable3, drawable4, drawable5);
        } else {
            textView.setCompoundDrawablesWithIntrinsicBounds(drawable2, drawable3, drawable4, drawable5);
        }
        if (b) {
            ((ea) a).startTransition(250);
        }
    }

    private boolean b(boolean z, boolean z2) {
        return this.AO && !z2 && (!z || this.AP);
    }

    public void L(int i) {
        this.AH = i;
    }

    void a(Context context, Bitmap bitmap, boolean z) {
        ed.d(bitmap);
        if ((this.AQ & 1) != 0) {
            bitmap = eb.a(bitmap);
        }
        a(new BitmapDrawable(context.getResources(), bitmap), z, false, true);
    }

    public void a(ImageView imageView) {
        ed.d(imageView);
        this.AK = null;
        this.AL = new WeakReference(imageView);
        this.AM = null;
        this.AN = -1;
        this.AI = 2;
        this.AJ = imageView.hashCode();
    }

    public void a(OnImageLoadedListener onImageLoadedListener) {
        ed.d(onImageLoadedListener);
        this.AK = new WeakReference(onImageLoadedListener);
        this.AL = null;
        this.AM = null;
        this.AN = -1;
        this.AI = 1;
        this.AJ = ep.hashCode(onImageLoadedListener, this.AF);
    }

    void b(Context context, boolean z) {
        Drawable drawable = null;
        if (this.AH != 0) {
            Resources resources = context.getResources();
            drawable = resources.getDrawable(this.AH);
            if ((this.AQ & 1) != 0) {
                drawable = eb.a(resources, drawable);
            }
        }
        a(drawable, z, false, false);
    }

    public boolean equals(Object obj) {
        if (obj instanceof a) {
            return this == obj || ((a) obj).hashCode() == hashCode();
        } else {
            return false;
        }
    }

    public int hashCode() {
        return this.AJ;
    }

    void x(Context context) {
        Drawable drawable = null;
        if (this.AG != 0) {
            Resources resources = context.getResources();
            drawable = resources.getDrawable(this.AG);
            if ((this.AQ & 1) != 0) {
                drawable = eb.a(resources, drawable);
            }
        }
        a(drawable, false, true, false);
    }
}
