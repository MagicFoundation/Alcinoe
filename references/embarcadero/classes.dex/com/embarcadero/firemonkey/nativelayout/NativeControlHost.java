package com.embarcadero.firemonkey.nativelayout;

import android.content.Context;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.LinearLayout.LayoutParams;

public class NativeControlHost {
    private View mHostedView;
    private LayoutParams mLayoutParams;
    private LinearLayout mLinearLayout;

    public NativeControlHost(Context context) {
        this.mLinearLayout = new LinearLayout(context);
        this.mLayoutParams = new LayoutParams(0, 0);
    }

    public void setControl(View view) {
        this.mHostedView = view;
        this.mLinearLayout.addView(this.mHostedView, this.mLayoutParams);
    }

    public void setPosition(int absoluteX, int absoluteY) {
        this.mLayoutParams.leftMargin = absoluteX;
        this.mLayoutParams.topMargin = absoluteY;
        if (this.mHostedView != null) {
            this.mLinearLayout.updateViewLayout(this.mHostedView, this.mLayoutParams);
        }
    }

    public void setSize(int absoluteWidth, int absoluteHeight) {
        this.mLayoutParams.width = absoluteWidth;
        this.mLayoutParams.height = absoluteHeight;
        if (this.mHostedView != null) {
            this.mLinearLayout.updateViewLayout(this.mHostedView, this.mLayoutParams);
        }
    }

    public void setFocus(boolean newFocusState) {
        if (this.mHostedView == null) {
            return;
        }
        if (newFocusState) {
            this.mHostedView.setFocusable(true);
            this.mHostedView.setFocusableInTouchMode(true);
            this.mHostedView.requestFocus();
            return;
        }
        this.mHostedView.clearFocus();
    }

    public void setFocusable(boolean focusable) {
        this.mHostedView.setFocusable(focusable);
        this.mHostedView.setFocusableInTouchMode(focusable);
        if (!focusable) {
            this.mHostedView.clearFocus();
        }
    }

    public View getView() {
        return this.mLinearLayout;
    }
}
