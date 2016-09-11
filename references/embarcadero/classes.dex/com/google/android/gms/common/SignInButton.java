package com.google.android.gms.common;

import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.FrameLayout;
import com.google.android.gms.dynamic.e.a;
import com.google.android.gms.internal.er;
import com.google.android.gms.internal.es;
import com.google.android.gms.internal.et;

public final class SignInButton extends FrameLayout implements OnClickListener {
    public static final int COLOR_DARK = 0;
    public static final int COLOR_LIGHT = 1;
    public static final int SIZE_ICON_ONLY = 2;
    public static final int SIZE_STANDARD = 0;
    public static final int SIZE_WIDE = 1;
    private int mSize;
    private int yX;
    private View yY;
    private OnClickListener yZ;

    public SignInButton(Context context) {
        this(context, null);
    }

    public SignInButton(Context context, AttributeSet attrs) {
        this(context, attrs, SIZE_STANDARD);
    }

    public SignInButton(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        this.yZ = null;
        setStyle(SIZE_STANDARD, SIZE_STANDARD);
    }

    private static Button c(Context context, int i, int i2) {
        Button etVar = new et(context);
        etVar.a(context.getResources(), i, i2);
        return etVar;
    }

    private void v(Context context) {
        if (this.yY != null) {
            removeView(this.yY);
        }
        try {
            this.yY = es.d(context, this.mSize, this.yX);
        } catch (a e) {
            Log.w("SignInButton", "Sign in button not found, using placeholder instead");
            this.yY = c(context, this.mSize, this.yX);
        }
        addView(this.yY);
        this.yY.setEnabled(isEnabled());
        this.yY.setOnClickListener(this);
    }

    public void onClick(View view) {
        if (this.yZ != null && view == this.yY) {
            this.yZ.onClick(this);
        }
    }

    public void setColorScheme(int colorScheme) {
        setStyle(this.mSize, colorScheme);
    }

    public void setEnabled(boolean enabled) {
        super.setEnabled(enabled);
        this.yY.setEnabled(enabled);
    }

    public void setOnClickListener(OnClickListener listener) {
        this.yZ = listener;
        if (this.yY != null) {
            this.yY.setOnClickListener(this);
        }
    }

    public void setSize(int buttonSize) {
        setStyle(buttonSize, this.yX);
    }

    public void setStyle(int buttonSize, int colorScheme) {
        boolean z = true;
        boolean z2 = buttonSize >= 0 && buttonSize < 3;
        er.a(z2, "Unknown button size " + buttonSize);
        if (colorScheme < 0 || colorScheme >= SIZE_ICON_ONLY) {
            z = false;
        }
        er.a(z, "Unknown color scheme " + colorScheme);
        this.mSize = buttonSize;
        this.yX = colorScheme;
        v(getContext());
    }
}
