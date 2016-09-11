package android.support.v4.view.accessibility;

import android.os.Build.VERSION;
import android.view.accessibility.AccessibilityEvent;

public class AccessibilityEventCompat {
    private static final AccessibilityEventVersionImpl IMPL;
    public static final int TYPES_ALL_MASK = -1;
    public static final int TYPE_ANNOUNCEMENT = 16384;
    public static final int TYPE_GESTURE_DETECTION_END = 524288;
    public static final int TYPE_GESTURE_DETECTION_START = 262144;
    public static final int TYPE_TOUCH_EXPLORATION_GESTURE_END = 1024;
    public static final int TYPE_TOUCH_EXPLORATION_GESTURE_START = 512;
    public static final int TYPE_TOUCH_INTERACTION_END = 2097152;
    public static final int TYPE_TOUCH_INTERACTION_START = 1048576;
    public static final int TYPE_VIEW_ACCESSIBILITY_FOCUSED = 32768;
    public static final int TYPE_VIEW_ACCESSIBILITY_FOCUS_CLEARED = 65536;
    public static final int TYPE_VIEW_HOVER_ENTER = 128;
    public static final int TYPE_VIEW_HOVER_EXIT = 256;
    public static final int TYPE_VIEW_SCROLLED = 4096;
    public static final int TYPE_VIEW_TEXT_SELECTION_CHANGED = 8192;
    public static final int TYPE_VIEW_TEXT_TRAVERSED_AT_MOVEMENT_GRANULARITY = 131072;
    public static final int TYPE_WINDOW_CONTENT_CHANGED = 2048;

    interface AccessibilityEventVersionImpl {
        void appendRecord(AccessibilityEvent accessibilityEvent, Object obj);

        Object getRecord(AccessibilityEvent accessibilityEvent, int i);

        int getRecordCount(AccessibilityEvent accessibilityEvent);
    }

    static class AccessibilityEventStubImpl implements AccessibilityEventVersionImpl {
        AccessibilityEventStubImpl() {
        }

        public void appendRecord(AccessibilityEvent event, Object record) {
        }

        public Object getRecord(AccessibilityEvent event, int index) {
            return null;
        }

        public int getRecordCount(AccessibilityEvent event) {
            return 0;
        }
    }

    static class AccessibilityEventIcsImpl extends AccessibilityEventStubImpl {
        AccessibilityEventIcsImpl() {
        }

        public void appendRecord(AccessibilityEvent event, Object record) {
            AccessibilityEventCompatIcs.appendRecord(event, record);
        }

        public Object getRecord(AccessibilityEvent event, int index) {
            return AccessibilityEventCompatIcs.getRecord(event, index);
        }

        public int getRecordCount(AccessibilityEvent event) {
            return AccessibilityEventCompatIcs.getRecordCount(event);
        }
    }

    static {
        if (VERSION.SDK_INT >= 14) {
            IMPL = new AccessibilityEventIcsImpl();
        } else {
            IMPL = new AccessibilityEventStubImpl();
        }
    }

    private AccessibilityEventCompat() {
    }

    public static int getRecordCount(AccessibilityEvent event) {
        return IMPL.getRecordCount(event);
    }

    public static void appendRecord(AccessibilityEvent event, AccessibilityRecordCompat record) {
        IMPL.appendRecord(event, record.getImpl());
    }

    public static AccessibilityRecordCompat getRecord(AccessibilityEvent event, int index) {
        return new AccessibilityRecordCompat(IMPL.getRecord(event, index));
    }

    public static AccessibilityRecordCompat asRecord(AccessibilityEvent event) {
        return new AccessibilityRecordCompat(event);
    }
}
