package android.support.v4.graphics.drawable;

import android.graphics.drawable.Drawable;

class DrawableCompatHoneycomb {
    DrawableCompatHoneycomb() {
    }

    public static void jumpToCurrentState(Drawable drawable) {
        drawable.jumpToCurrentState();
    }
}
