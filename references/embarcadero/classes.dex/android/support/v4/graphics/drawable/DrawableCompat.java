package android.support.v4.graphics.drawable;

import android.graphics.drawable.Drawable;
import android.os.Build.VERSION;

public class DrawableCompat {
    static final DrawableImpl IMPL;

    interface DrawableImpl {
        boolean isAutoMirrored(Drawable drawable);

        void jumpToCurrentState(Drawable drawable);

        void setAutoMirrored(Drawable drawable, boolean z);
    }

    static class BaseDrawableImpl implements DrawableImpl {
        BaseDrawableImpl() {
        }

        public void jumpToCurrentState(Drawable drawable) {
        }

        public void setAutoMirrored(Drawable drawable, boolean mirrored) {
        }

        public boolean isAutoMirrored(Drawable drawable) {
            return false;
        }
    }

    static class HoneycombDrawableImpl extends BaseDrawableImpl {
        HoneycombDrawableImpl() {
        }

        public void jumpToCurrentState(Drawable drawable) {
            DrawableCompatHoneycomb.jumpToCurrentState(drawable);
        }
    }

    static class KitKatDrawableImpl extends HoneycombDrawableImpl {
        KitKatDrawableImpl() {
        }

        public void setAutoMirrored(Drawable drawable, boolean mirrored) {
            DrawableCompatKitKat.setAutoMirrored(drawable, mirrored);
        }

        public boolean isAutoMirrored(Drawable drawable) {
            return DrawableCompatKitKat.isAutoMirrored(drawable);
        }
    }

    static {
        int version = VERSION.SDK_INT;
        if (version >= 19) {
            IMPL = new KitKatDrawableImpl();
        } else if (version >= 11) {
            IMPL = new HoneycombDrawableImpl();
        } else {
            IMPL = new BaseDrawableImpl();
        }
    }

    public static void jumpToCurrentState(Drawable drawable) {
        IMPL.jumpToCurrentState(drawable);
    }

    public static void setAutoMirrored(Drawable drawable, boolean mirrored) {
        IMPL.setAutoMirrored(drawable, mirrored);
    }

    public static boolean isAutoMirrored(Drawable drawable) {
        return IMPL.isAutoMirrored(drawable);
    }
}
