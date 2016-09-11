package android.support.v4.view;

import android.graphics.Paint;
import android.view.View;

class ViewCompatJellybeanMr1 {
    ViewCompatJellybeanMr1() {
    }

    public static int getLabelFor(View view) {
        return view.getLabelFor();
    }

    public static void setLabelFor(View view, int id) {
        view.setLabelFor(id);
    }

    public static void setLayerPaint(View view, Paint paint) {
        view.setLayerPaint(paint);
    }

    public static int getLayoutDirection(View view) {
        return view.getLayoutDirection();
    }

    public static void setLayoutDirection(View view, int layoutDirection) {
        view.setLayoutDirection(layoutDirection);
    }
}
