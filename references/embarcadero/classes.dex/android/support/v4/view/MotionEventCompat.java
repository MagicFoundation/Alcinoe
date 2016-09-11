package android.support.v4.view;

import android.os.Build.VERSION;
import android.view.MotionEvent;

public class MotionEventCompat {
    public static final int ACTION_HOVER_ENTER = 9;
    public static final int ACTION_HOVER_EXIT = 10;
    public static final int ACTION_HOVER_MOVE = 7;
    public static final int ACTION_MASK = 255;
    public static final int ACTION_POINTER_DOWN = 5;
    public static final int ACTION_POINTER_INDEX_MASK = 65280;
    public static final int ACTION_POINTER_INDEX_SHIFT = 8;
    public static final int ACTION_POINTER_UP = 6;
    public static final int ACTION_SCROLL = 8;
    static final MotionEventVersionImpl IMPL;

    interface MotionEventVersionImpl {
        int findPointerIndex(MotionEvent motionEvent, int i);

        int getPointerCount(MotionEvent motionEvent);

        int getPointerId(MotionEvent motionEvent, int i);

        float getX(MotionEvent motionEvent, int i);

        float getY(MotionEvent motionEvent, int i);
    }

    static class BaseMotionEventVersionImpl implements MotionEventVersionImpl {
        BaseMotionEventVersionImpl() {
        }

        public int findPointerIndex(MotionEvent event, int pointerId) {
            if (pointerId == 0) {
                return 0;
            }
            return -1;
        }

        public int getPointerId(MotionEvent event, int pointerIndex) {
            if (pointerIndex == 0) {
                return 0;
            }
            throw new IndexOutOfBoundsException("Pre-Eclair does not support multiple pointers");
        }

        public float getX(MotionEvent event, int pointerIndex) {
            if (pointerIndex == 0) {
                return event.getX();
            }
            throw new IndexOutOfBoundsException("Pre-Eclair does not support multiple pointers");
        }

        public float getY(MotionEvent event, int pointerIndex) {
            if (pointerIndex == 0) {
                return event.getY();
            }
            throw new IndexOutOfBoundsException("Pre-Eclair does not support multiple pointers");
        }

        public int getPointerCount(MotionEvent event) {
            return 1;
        }
    }

    static class EclairMotionEventVersionImpl implements MotionEventVersionImpl {
        EclairMotionEventVersionImpl() {
        }

        public int findPointerIndex(MotionEvent event, int pointerId) {
            return MotionEventCompatEclair.findPointerIndex(event, pointerId);
        }

        public int getPointerId(MotionEvent event, int pointerIndex) {
            return MotionEventCompatEclair.getPointerId(event, pointerIndex);
        }

        public float getX(MotionEvent event, int pointerIndex) {
            return MotionEventCompatEclair.getX(event, pointerIndex);
        }

        public float getY(MotionEvent event, int pointerIndex) {
            return MotionEventCompatEclair.getY(event, pointerIndex);
        }

        public int getPointerCount(MotionEvent event) {
            return MotionEventCompatEclair.getPointerCount(event);
        }
    }

    static {
        if (VERSION.SDK_INT >= ACTION_POINTER_DOWN) {
            IMPL = new EclairMotionEventVersionImpl();
        } else {
            IMPL = new BaseMotionEventVersionImpl();
        }
    }

    public static int getActionMasked(MotionEvent event) {
        return event.getAction() & ACTION_MASK;
    }

    public static int getActionIndex(MotionEvent event) {
        return (event.getAction() & ACTION_POINTER_INDEX_MASK) >> ACTION_SCROLL;
    }

    public static int findPointerIndex(MotionEvent event, int pointerId) {
        return IMPL.findPointerIndex(event, pointerId);
    }

    public static int getPointerId(MotionEvent event, int pointerIndex) {
        return IMPL.getPointerId(event, pointerIndex);
    }

    public static float getX(MotionEvent event, int pointerIndex) {
        return IMPL.getX(event, pointerIndex);
    }

    public static float getY(MotionEvent event, int pointerIndex) {
        return IMPL.getY(event, pointerIndex);
    }

    public static int getPointerCount(MotionEvent event) {
        return IMPL.getPointerCount(event);
    }
}
