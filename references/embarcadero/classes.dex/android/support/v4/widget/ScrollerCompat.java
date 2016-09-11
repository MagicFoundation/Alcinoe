package android.support.v4.widget;

import android.content.Context;
import android.os.Build.VERSION;
import android.view.animation.Interpolator;
import android.widget.Scroller;

public class ScrollerCompat {
    static final ScrollerCompatImpl IMPL;
    Object mScroller;

    interface ScrollerCompatImpl {
        void abortAnimation(Object obj);

        boolean computeScrollOffset(Object obj);

        Object createScroller(Context context, Interpolator interpolator);

        void fling(Object obj, int i, int i2, int i3, int i4, int i5, int i6, int i7, int i8);

        void fling(Object obj, int i, int i2, int i3, int i4, int i5, int i6, int i7, int i8, int i9, int i10);

        float getCurrVelocity(Object obj);

        int getCurrX(Object obj);

        int getCurrY(Object obj);

        int getFinalX(Object obj);

        int getFinalY(Object obj);

        boolean isFinished(Object obj);

        boolean isOverScrolled(Object obj);

        void notifyHorizontalEdgeReached(Object obj, int i, int i2, int i3);

        void notifyVerticalEdgeReached(Object obj, int i, int i2, int i3);

        void startScroll(Object obj, int i, int i2, int i3, int i4);

        void startScroll(Object obj, int i, int i2, int i3, int i4, int i5);
    }

    static class ScrollerCompatImplBase implements ScrollerCompatImpl {
        ScrollerCompatImplBase() {
        }

        public Object createScroller(Context context, Interpolator interpolator) {
            return interpolator != null ? new Scroller(context, interpolator) : new Scroller(context);
        }

        public boolean isFinished(Object scroller) {
            return ((Scroller) scroller).isFinished();
        }

        public int getCurrX(Object scroller) {
            return ((Scroller) scroller).getCurrX();
        }

        public int getCurrY(Object scroller) {
            return ((Scroller) scroller).getCurrY();
        }

        public float getCurrVelocity(Object scroller) {
            return 0.0f;
        }

        public boolean computeScrollOffset(Object scroller) {
            return ((Scroller) scroller).computeScrollOffset();
        }

        public void startScroll(Object scroller, int startX, int startY, int dx, int dy) {
            ((Scroller) scroller).startScroll(startX, startY, dx, dy);
        }

        public void startScroll(Object scroller, int startX, int startY, int dx, int dy, int duration) {
            ((Scroller) scroller).startScroll(startX, startY, dx, dy, duration);
        }

        public void fling(Object scroller, int startX, int startY, int velX, int velY, int minX, int maxX, int minY, int maxY) {
            ((Scroller) scroller).fling(startX, startY, velX, velY, minX, maxX, minY, maxY);
        }

        public void fling(Object scroller, int startX, int startY, int velX, int velY, int minX, int maxX, int minY, int maxY, int overX, int overY) {
            ((Scroller) scroller).fling(startX, startY, velX, velY, minX, maxX, minY, maxY);
        }

        public void abortAnimation(Object scroller) {
            ((Scroller) scroller).abortAnimation();
        }

        public void notifyHorizontalEdgeReached(Object scroller, int startX, int finalX, int overX) {
        }

        public void notifyVerticalEdgeReached(Object scroller, int startY, int finalY, int overY) {
        }

        public boolean isOverScrolled(Object scroller) {
            return false;
        }

        public int getFinalX(Object scroller) {
            return ((Scroller) scroller).getFinalX();
        }

        public int getFinalY(Object scroller) {
            return ((Scroller) scroller).getFinalY();
        }
    }

    static class ScrollerCompatImplGingerbread implements ScrollerCompatImpl {
        ScrollerCompatImplGingerbread() {
        }

        public Object createScroller(Context context, Interpolator interpolator) {
            return ScrollerCompatGingerbread.createScroller(context, interpolator);
        }

        public boolean isFinished(Object scroller) {
            return ScrollerCompatGingerbread.isFinished(scroller);
        }

        public int getCurrX(Object scroller) {
            return ScrollerCompatGingerbread.getCurrX(scroller);
        }

        public int getCurrY(Object scroller) {
            return ScrollerCompatGingerbread.getCurrY(scroller);
        }

        public float getCurrVelocity(Object scroller) {
            return 0.0f;
        }

        public boolean computeScrollOffset(Object scroller) {
            return ScrollerCompatGingerbread.computeScrollOffset(scroller);
        }

        public void startScroll(Object scroller, int startX, int startY, int dx, int dy) {
            ScrollerCompatGingerbread.startScroll(scroller, startX, startY, dx, dy);
        }

        public void startScroll(Object scroller, int startX, int startY, int dx, int dy, int duration) {
            ScrollerCompatGingerbread.startScroll(scroller, startX, startY, dx, dy, duration);
        }

        public void fling(Object scroller, int startX, int startY, int velX, int velY, int minX, int maxX, int minY, int maxY) {
            ScrollerCompatGingerbread.fling(scroller, startX, startY, velX, velY, minX, maxX, minY, maxY);
        }

        public void fling(Object scroller, int startX, int startY, int velX, int velY, int minX, int maxX, int minY, int maxY, int overX, int overY) {
            ScrollerCompatGingerbread.fling(scroller, startX, startY, velX, velY, minX, maxX, minY, maxY, overX, overY);
        }

        public void abortAnimation(Object scroller) {
            ScrollerCompatGingerbread.abortAnimation(scroller);
        }

        public void notifyHorizontalEdgeReached(Object scroller, int startX, int finalX, int overX) {
            ScrollerCompatGingerbread.notifyHorizontalEdgeReached(scroller, startX, finalX, overX);
        }

        public void notifyVerticalEdgeReached(Object scroller, int startY, int finalY, int overY) {
            ScrollerCompatGingerbread.notifyVerticalEdgeReached(scroller, startY, finalY, overY);
        }

        public boolean isOverScrolled(Object scroller) {
            return ScrollerCompatGingerbread.isOverScrolled(scroller);
        }

        public int getFinalX(Object scroller) {
            return ScrollerCompatGingerbread.getFinalX(scroller);
        }

        public int getFinalY(Object scroller) {
            return ScrollerCompatGingerbread.getFinalY(scroller);
        }
    }

    static class ScrollerCompatImplIcs extends ScrollerCompatImplGingerbread {
        ScrollerCompatImplIcs() {
        }

        public float getCurrVelocity(Object scroller) {
            return ScrollerCompatIcs.getCurrVelocity(scroller);
        }
    }

    static {
        int version = VERSION.SDK_INT;
        if (version >= 14) {
            IMPL = new ScrollerCompatImplIcs();
        } else if (version >= 9) {
            IMPL = new ScrollerCompatImplGingerbread();
        } else {
            IMPL = new ScrollerCompatImplBase();
        }
    }

    public static ScrollerCompat create(Context context) {
        return create(context, null);
    }

    public static ScrollerCompat create(Context context, Interpolator interpolator) {
        return new ScrollerCompat(context, interpolator);
    }

    ScrollerCompat(Context context, Interpolator interpolator) {
        this.mScroller = IMPL.createScroller(context, interpolator);
    }

    public boolean isFinished() {
        return IMPL.isFinished(this.mScroller);
    }

    public int getCurrX() {
        return IMPL.getCurrX(this.mScroller);
    }

    public int getCurrY() {
        return IMPL.getCurrY(this.mScroller);
    }

    public int getFinalX() {
        return IMPL.getFinalX(this.mScroller);
    }

    public int getFinalY() {
        return IMPL.getFinalY(this.mScroller);
    }

    public float getCurrVelocity() {
        return IMPL.getCurrVelocity(this.mScroller);
    }

    public boolean computeScrollOffset() {
        return IMPL.computeScrollOffset(this.mScroller);
    }

    public void startScroll(int startX, int startY, int dx, int dy) {
        IMPL.startScroll(this.mScroller, startX, startY, dx, dy);
    }

    public void startScroll(int startX, int startY, int dx, int dy, int duration) {
        IMPL.startScroll(this.mScroller, startX, startY, dx, dy, duration);
    }

    public void fling(int startX, int startY, int velocityX, int velocityY, int minX, int maxX, int minY, int maxY) {
        IMPL.fling(this.mScroller, startX, startY, velocityX, velocityY, minX, maxX, minY, maxY);
    }

    public void fling(int startX, int startY, int velocityX, int velocityY, int minX, int maxX, int minY, int maxY, int overX, int overY) {
        IMPL.fling(this.mScroller, startX, startY, velocityX, velocityY, minX, maxX, minY, maxY, overX, overY);
    }

    public void abortAnimation() {
        IMPL.abortAnimation(this.mScroller);
    }

    public void notifyHorizontalEdgeReached(int startX, int finalX, int overX) {
        IMPL.notifyHorizontalEdgeReached(this.mScroller, startX, finalX, overX);
    }

    public void notifyVerticalEdgeReached(int startY, int finalY, int overY) {
        IMPL.notifyVerticalEdgeReached(this.mScroller, startY, finalY, overY);
    }

    public boolean isOverScrolled() {
        return IMPL.isOverScrolled(this.mScroller);
    }
}
