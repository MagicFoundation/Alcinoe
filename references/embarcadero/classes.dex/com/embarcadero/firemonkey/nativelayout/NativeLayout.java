package com.embarcadero.firemonkey.nativelayout;

import android.content.Context;
import android.os.IBinder;
import android.util.Log;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnFocusChangeListener;
import android.widget.LinearLayout;
import android.widget.LinearLayout.LayoutParams;
import com.embarcadero.firemonkey.FMXNativeActivity;
import com.google.android.gms.location.DetectedActivity;

public class NativeLayout {
    private static final String TAG = "NativeLayout";
    private Context mContext;
    private LinearLayout mLayout;
    private OnFocusChangeListener mOnFocusChangeListener;
    private IBinder mParent;

    class LinearLayout2 extends LinearLayout {
        public LinearLayout2(Context context) {
            super(context);
            setFocusable(false);
            setEnabled(false);
        }

        public boolean dispatchKeyEvent(KeyEvent ev) {
            if (ev.getAction() != 0) {
                return false;
            }
            switch (ev.getKeyCode()) {
                case DetectedActivity.UNKNOWN /*4*/:
                case 82:
                    ((FMXNativeActivity) NativeLayout.this.mContext).getViewStack().disableViews();
                    return false;
                default:
                    return super.dispatchKeyEvent(ev);
            }
        }

        public boolean dispatchTouchEvent(MotionEvent event) {
            MotionEvent ev = event;
            ((FMXNativeActivity) NativeLayout.this.mContext).getTextEditorProxy().setFocusable(false);
            ((FMXNativeActivity) NativeLayout.this.mContext).getTextEditorProxy().showSoftInput(false);
            getHandler().postDelayed(new Runnable() {
                public void run() {
                    ((FMXNativeActivity) NativeLayout.this.mContext).getViewStack().addView(LinearLayout2.this);
                    if (NativeLayout.this.mOnFocusChangeListener != null) {
                        NativeLayout.this.mOnFocusChangeListener.onFocusChange(LinearLayout2.this, true);
                    }
                }
            }, 250);
            return super.dispatchTouchEvent(ev);
        }
    }

    public NativeLayout(Context Con, IBinder Token) {
        this.mContext = Con;
        this.mParent = Token;
    }

    public void setControl(View view) {
        if (view != null) {
            this.mLayout = new LinearLayout2(this.mContext);
            this.mLayout.addView(view, new LayoutParams(-1, -1));
            ((FMXNativeActivity) this.mContext).getViewStack().addView(this.mLayout);
            return;
        }
        this.mLayout.removeAllViews();
        ((FMXNativeActivity) this.mContext).getViewStack().removeView(this.mLayout);
        this.mLayout = null;
    }

    public void setPosition(int absoluteX, int absoluteY) {
        Log.d(TAG, "setPosition " + absoluteX + " " + absoluteY + " mLayout=" + this.mLayout);
        if (this.mLayout != null) {
            ((FMXNativeActivity) this.mContext).getViewStack().setPosition(this.mLayout, absoluteX, absoluteY);
        }
    }

    public void setSize(int absoluteWidth, int absoluteHeight) {
        if (this.mLayout != null) {
            ((FMXNativeActivity) this.mContext).getViewStack().setSize(this.mLayout, absoluteWidth, absoluteHeight);
        }
    }

    public void setFocus(boolean newFocusState) {
    }

    public void setOnFocusChangeListener(OnFocusChangeListener listener) {
        this.mOnFocusChangeListener = listener;
    }
}
