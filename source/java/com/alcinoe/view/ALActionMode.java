package com.alcinoe.view;

import android.graphics.Rect;
import android.view.ActionMode;
import android.view.View;

/**
 * dummy interface to expose Callback2 in android api < 23
 */
public abstract class ALActionMode {

    /**
     * Extension of {@link ActionMode.Callback} to provide content rect information. This is
     * required for ActionModes with dynamic positioning such as the ones with type
     * {@link ActionMode#TYPE_FLOATING} to ensure the positioning doesn't obscure app content. If
     * an app fails to provide a subclass of this class, a default implementation will be used.
     */
    public static abstract class Callback2 implements ActionMode.Callback {

        /**
         * Called when an ActionMode needs to be positioned on screen, potentially occluding view
         * content. Note this may be called on a per-frame basis.
         *
         * @param mode The ActionMode that requires positioning.
         * @param view The View that originated the ActionMode, in whose coordinates the Rect should
         *          be provided.
         * @param outRect The Rect to be populated with the content position. Use this to specify
         *          where the content in your app lives within the given view. This will be used
         *          to avoid occluding the given content Rect with the created ActionMode.
         */
        public void onGetContentRect(ActionMode mode, View view, Rect outRect) {
            if (view != null) {
                outRect.set(0, 0, view.getWidth(), view.getHeight());
            } else {
                outRect.set(0, 0, 0, 0);
            }
        }

    }
}
