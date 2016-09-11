package android.support.v4.view;

import android.os.Build.VERSION;
import android.view.View;
import android.view.ViewGroup;
import android.view.accessibility.AccessibilityEvent;

public class ViewGroupCompat {
    static final ViewGroupCompatImpl IMPL;
    public static final int LAYOUT_MODE_CLIP_BOUNDS = 0;
    public static final int LAYOUT_MODE_OPTICAL_BOUNDS = 1;

    interface ViewGroupCompatImpl {
        int getLayoutMode(ViewGroup viewGroup);

        boolean onRequestSendAccessibilityEvent(ViewGroup viewGroup, View view, AccessibilityEvent accessibilityEvent);

        void setLayoutMode(ViewGroup viewGroup, int i);

        void setMotionEventSplittingEnabled(ViewGroup viewGroup, boolean z);
    }

    static class ViewGroupCompatStubImpl implements ViewGroupCompatImpl {
        ViewGroupCompatStubImpl() {
        }

        public boolean onRequestSendAccessibilityEvent(ViewGroup group, View child, AccessibilityEvent event) {
            return true;
        }

        public void setMotionEventSplittingEnabled(ViewGroup group, boolean split) {
        }

        public int getLayoutMode(ViewGroup group) {
            return ViewGroupCompat.LAYOUT_MODE_CLIP_BOUNDS;
        }

        public void setLayoutMode(ViewGroup group, int mode) {
        }
    }

    static class ViewGroupCompatHCImpl extends ViewGroupCompatStubImpl {
        ViewGroupCompatHCImpl() {
        }

        public void setMotionEventSplittingEnabled(ViewGroup group, boolean split) {
            ViewGroupCompatHC.setMotionEventSplittingEnabled(group, split);
        }
    }

    static class ViewGroupCompatIcsImpl extends ViewGroupCompatHCImpl {
        ViewGroupCompatIcsImpl() {
        }

        public boolean onRequestSendAccessibilityEvent(ViewGroup group, View child, AccessibilityEvent event) {
            return ViewGroupCompatIcs.onRequestSendAccessibilityEvent(group, child, event);
        }
    }

    static class ViewGroupCompatJellybeanMR2Impl extends ViewGroupCompatIcsImpl {
        ViewGroupCompatJellybeanMR2Impl() {
        }

        public int getLayoutMode(ViewGroup group) {
            return ViewGroupCompatJellybeanMR2.getLayoutMode(group);
        }

        public void setLayoutMode(ViewGroup group, int mode) {
            ViewGroupCompatJellybeanMR2.setLayoutMode(group, mode);
        }
    }

    static {
        int version = VERSION.SDK_INT;
        if (version >= 18) {
            IMPL = new ViewGroupCompatJellybeanMR2Impl();
        } else if (version >= 14) {
            IMPL = new ViewGroupCompatIcsImpl();
        } else if (version >= 11) {
            IMPL = new ViewGroupCompatHCImpl();
        } else {
            IMPL = new ViewGroupCompatStubImpl();
        }
    }

    private ViewGroupCompat() {
    }

    public static boolean onRequestSendAccessibilityEvent(ViewGroup group, View child, AccessibilityEvent event) {
        return IMPL.onRequestSendAccessibilityEvent(group, child, event);
    }

    public static void setMotionEventSplittingEnabled(ViewGroup group, boolean split) {
        IMPL.setMotionEventSplittingEnabled(group, split);
    }

    public static int getLayoutMode(ViewGroup group) {
        return IMPL.getLayoutMode(group);
    }

    public static void setLayoutMode(ViewGroup group, int mode) {
        IMPL.setLayoutMode(group, mode);
    }
}
