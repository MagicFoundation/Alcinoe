package android.support.v4.widget;

import android.content.Context;
import android.view.animation.Interpolator;
import android.widget.OverScroller;

class ScrollerCompatGingerbread {
    ScrollerCompatGingerbread() {
    }

    public static Object createScroller(Context context, Interpolator interpolator) {
        return interpolator != null ? new OverScroller(context, interpolator) : new OverScroller(context);
    }

    public static boolean isFinished(Object scroller) {
        return ((OverScroller) scroller).isFinished();
    }

    public static int getCurrX(Object scroller) {
        return ((OverScroller) scroller).getCurrX();
    }

    public static int getCurrY(Object scroller) {
        return ((OverScroller) scroller).getCurrY();
    }

    public static boolean computeScrollOffset(Object scroller) {
        return ((OverScroller) scroller).computeScrollOffset();
    }

    public static void startScroll(Object scroller, int startX, int startY, int dx, int dy) {
        ((OverScroller) scroller).startScroll(startX, startY, dx, dy);
    }

    public static void startScroll(Object scroller, int startX, int startY, int dx, int dy, int duration) {
        ((OverScroller) scroller).startScroll(startX, startY, dx, dy, duration);
    }

    public static void fling(Object scroller, int startX, int startY, int velX, int velY, int minX, int maxX, int minY, int maxY) {
        ((OverScroller) scroller).fling(startX, startY, velX, velY, minX, maxX, minY, maxY);
    }

    public static void fling(Object scroller, int startX, int startY, int velX, int velY, int minX, int maxX, int minY, int maxY, int overX, int overY) {
        ((OverScroller) scroller).fling(startX, startY, velX, velY, minX, maxX, minY, maxY, overX, overY);
    }

    public static void abortAnimation(Object scroller) {
        ((OverScroller) scroller).abortAnimation();
    }

    public static void notifyHorizontalEdgeReached(Object scroller, int startX, int finalX, int overX) {
        ((OverScroller) scroller).notifyHorizontalEdgeReached(startX, finalX, overX);
    }

    public static void notifyVerticalEdgeReached(Object scroller, int startY, int finalY, int overY) {
        ((OverScroller) scroller).notifyVerticalEdgeReached(startY, finalY, overY);
    }

    public static boolean isOverScrolled(Object scroller) {
        return ((OverScroller) scroller).isOverScrolled();
    }

    public static int getFinalX(Object scroller) {
        return ((OverScroller) scroller).getFinalX();
    }

    public static int getFinalY(Object scroller) {
        return ((OverScroller) scroller).getFinalY();
    }
}
