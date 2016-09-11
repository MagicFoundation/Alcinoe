package com.google.android.gms.plus;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.util.AttributeSet;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.FrameLayout;
import com.google.android.gms.internal.er;
import com.google.android.gms.internal.eu;
import com.google.android.gms.plus.internal.g;

public final class PlusOneButton extends FrameLayout {
    public static final int ANNOTATION_BUBBLE = 1;
    public static final int ANNOTATION_INLINE = 2;
    public static final int ANNOTATION_NONE = 0;
    public static final int DEFAULT_ACTIVITY_REQUEST_CODE = -1;
    public static final int SIZE_MEDIUM = 1;
    public static final int SIZE_SMALL = 0;
    public static final int SIZE_STANDARD = 3;
    public static final int SIZE_TALL = 2;
    private View QV;
    private int QW;
    private int QX;
    private OnPlusOneClickListener QY;
    private int mSize;
    private String pS;

    public interface OnPlusOneClickListener {
        void onPlusOneClick(Intent intent);
    }

    protected class DefaultOnPlusOneClickListener implements OnClickListener, OnPlusOneClickListener {
        private final OnPlusOneClickListener QZ;
        final /* synthetic */ PlusOneButton Ra;

        public DefaultOnPlusOneClickListener(PlusOneButton plusOneButton, OnPlusOneClickListener proxy) {
            this.Ra = plusOneButton;
            this.QZ = proxy;
        }

        public void onClick(View view) {
            Intent intent = (Intent) this.Ra.QV.getTag();
            if (this.QZ != null) {
                this.QZ.onPlusOneClick(intent);
            } else {
                onPlusOneClick(intent);
            }
        }

        public void onPlusOneClick(Intent intent) {
            Context context = this.Ra.getContext();
            if ((context instanceof Activity) && intent != null) {
                ((Activity) context).startActivityForResult(intent, this.Ra.QX);
            }
        }
    }

    public PlusOneButton(Context context) {
        this(context, null);
    }

    public PlusOneButton(Context context, AttributeSet attrs) {
        super(context, attrs);
        this.mSize = getSize(context, attrs);
        this.QW = getAnnotation(context, attrs);
        this.QX = DEFAULT_ACTIVITY_REQUEST_CODE;
        v(getContext());
        if (!isInEditMode()) {
        }
    }

    protected static int getAnnotation(Context context, AttributeSet attrs) {
        String a = eu.a("http://schemas.android.com/apk/lib/com.google.android.gms.plus", "annotation", context, attrs, true, false, "PlusOneButton");
        if ("INLINE".equalsIgnoreCase(a)) {
            return SIZE_TALL;
        }
        return !"NONE".equalsIgnoreCase(a) ? SIZE_MEDIUM : SIZE_SMALL;
    }

    protected static int getSize(Context context, AttributeSet attrs) {
        String a = eu.a("http://schemas.android.com/apk/lib/com.google.android.gms.plus", "size", context, attrs, true, false, "PlusOneButton");
        if ("SMALL".equalsIgnoreCase(a)) {
            return SIZE_SMALL;
        }
        if ("MEDIUM".equalsIgnoreCase(a)) {
            return SIZE_MEDIUM;
        }
        return "TALL".equalsIgnoreCase(a) ? SIZE_TALL : SIZE_STANDARD;
    }

    private void v(Context context) {
        if (this.QV != null) {
            removeView(this.QV);
        }
        this.QV = g.a(context, this.mSize, this.QW, this.pS, this.QX);
        setOnPlusOneClickListener(this.QY);
        addView(this.QV);
    }

    public void initialize(String url, int activityRequestCode) {
        er.a(getContext() instanceof Activity, "To use this method, the PlusOneButton must be placed in an Activity. Use initialize(String, OnPlusOneClickListener).");
        this.pS = url;
        this.QX = activityRequestCode;
        v(getContext());
    }

    public void initialize(String url, OnPlusOneClickListener plusOneClickListener) {
        this.pS = url;
        this.QX = SIZE_SMALL;
        v(getContext());
        setOnPlusOneClickListener(plusOneClickListener);
    }

    protected void onLayout(boolean changed, int left, int top, int right, int bottom) {
        this.QV.layout(SIZE_SMALL, SIZE_SMALL, right - left, bottom - top);
    }

    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        View view = this.QV;
        measureChild(view, widthMeasureSpec, heightMeasureSpec);
        setMeasuredDimension(view.getMeasuredWidth(), view.getMeasuredHeight());
    }

    public void setAnnotation(int annotation) {
        this.QW = annotation;
        v(getContext());
    }

    public void setOnPlusOneClickListener(OnPlusOneClickListener listener) {
        this.QY = listener;
        this.QV.setOnClickListener(new DefaultOnPlusOneClickListener(this, listener));
    }

    public void setSize(int size) {
        this.mSize = size;
        v(getContext());
    }
}
