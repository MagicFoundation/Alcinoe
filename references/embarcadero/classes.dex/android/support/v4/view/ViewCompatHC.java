package android.support.v4.view;

import android.animation.ValueAnimator;
import android.graphics.Paint;
import android.view.View;

class ViewCompatHC {
    ViewCompatHC() {
    }

    static long getFrameTime() {
        return ValueAnimator.getFrameDelay();
    }

    public static float getAlpha(View view) {
        return view.getAlpha();
    }

    public static void setLayerType(View view, int layerType, Paint paint) {
        view.setLayerType(layerType, paint);
    }

    public static int getLayerType(View view) {
        return view.getLayerType();
    }

    public static int resolveSizeAndState(int size, int measureSpec, int childMeasuredState) {
        return View.resolveSizeAndState(size, measureSpec, childMeasuredState);
    }

    public static int getMeasuredWidthAndState(View view) {
        return view.getMeasuredWidthAndState();
    }

    public static int getMeasuredHeightAndState(View view) {
        return view.getMeasuredHeightAndState();
    }

    public static int getMeasuredState(View view) {
        return view.getMeasuredState();
    }
}
