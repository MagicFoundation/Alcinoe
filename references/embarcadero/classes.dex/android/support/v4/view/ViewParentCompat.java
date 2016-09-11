package android.support.v4.view;

import android.os.Build.VERSION;
import android.view.View;
import android.view.ViewParent;
import android.view.accessibility.AccessibilityEvent;
import android.view.accessibility.AccessibilityManager;

public class ViewParentCompat {
    static final ViewParentCompatImpl IMPL;

    interface ViewParentCompatImpl {
        boolean requestSendAccessibilityEvent(ViewParent viewParent, View view, AccessibilityEvent accessibilityEvent);
    }

    static class ViewParentCompatStubImpl implements ViewParentCompatImpl {
        ViewParentCompatStubImpl() {
        }

        public boolean requestSendAccessibilityEvent(ViewParent parent, View child, AccessibilityEvent event) {
            if (child == null) {
                return false;
            }
            ((AccessibilityManager) child.getContext().getSystemService("accessibility")).sendAccessibilityEvent(event);
            return true;
        }
    }

    static class ViewParentCompatICSImpl extends ViewParentCompatStubImpl {
        ViewParentCompatICSImpl() {
        }

        public boolean requestSendAccessibilityEvent(ViewParent parent, View child, AccessibilityEvent event) {
            return ViewParentCompatICS.requestSendAccessibilityEvent(parent, child, event);
        }
    }

    static {
        if (VERSION.SDK_INT >= 14) {
            IMPL = new ViewParentCompatICSImpl();
        } else {
            IMPL = new ViewParentCompatStubImpl();
        }
    }

    private ViewParentCompat() {
    }

    public static boolean requestSendAccessibilityEvent(ViewParent parent, View child, AccessibilityEvent event) {
        return IMPL.requestSendAccessibilityEvent(parent, child, event);
    }
}
