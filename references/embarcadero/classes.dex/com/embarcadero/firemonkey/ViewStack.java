package com.embarcadero.firemonkey;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.view.WindowManager.LayoutParams;
import java.util.ArrayList;
import java.util.Iterator;

public class ViewStack {
    private static final int FOCUSABLE_FLAGS = 544;
    private static final int NOT_FOCUSABLE_FLAGS = 131592;
    private static final String TAG = "ViewStack";
    private ArrayList<View> mViews;
    private final WindowManager mWindowManager;

    public ViewStack(Context context) {
        this.mWindowManager = (WindowManager) context.getSystemService("window");
        this.mViews = new ArrayList();
    }

    public void addView(ViewGroup view) {
        if (view == null || this.mViews.contains(view)) {
            LayoutParams lp;
            Iterator i$ = this.mViews.iterator();
            while (i$.hasNext()) {
                View v = (View) i$.next();
                lp = getLayoutParams(v);
                lp.flags = NOT_FOCUSABLE_FLAGS;
                this.mWindowManager.updateViewLayout(v, lp);
            }
            if (view != null) {
                lp = getLayoutParams(view);
                lp.flags = FOCUSABLE_FLAGS;
                this.mWindowManager.updateViewLayout(view, lp);
                return;
            }
            return;
        }
        this.mWindowManager.addView(view, getLayoutParams(view));
        this.mViews.add(view);
    }

    public void removeView(ViewGroup view) {
        if (view != null) {
            this.mViews.remove(view);
            this.mWindowManager.removeView(view);
        }
    }

    public void disableViews() {
        Iterator i$ = this.mViews.iterator();
        while (i$.hasNext()) {
            View v = (View) i$.next();
            LayoutParams lp = getLayoutParams(v);
            lp.flags = NOT_FOCUSABLE_FLAGS;
            this.mWindowManager.updateViewLayout(v, lp);
        }
    }

    public void setPosition(ViewGroup view, int absoluteX, int absoluteY) {
        LayoutParams lp = getLayoutParams(view);
        lp.x = absoluteX;
        lp.y = absoluteY;
        if (this.mViews.contains(view)) {
            this.mWindowManager.updateViewLayout(view, lp);
        }
    }

    public void setSize(ViewGroup view, int absoluteWidth, int absoluteHeight) {
        LayoutParams lp = getLayoutParams(view);
        lp.height = absoluteHeight;
        lp.width = absoluteWidth;
        if (this.mViews.contains(view)) {
            this.mWindowManager.updateViewLayout(view, lp);
        }
    }

    protected LayoutParams getLayoutParams(View view) {
        LayoutParams lp = (LayoutParams) view.getTag();
        if (lp != null) {
            return lp;
        }
        lp = createLayoutParams();
        view.setTag(lp);
        return lp;
    }

    protected LayoutParams createLayoutParams() {
        LayoutParams p = new LayoutParams();
        p.gravity = 51;
        p.width = 0;
        p.height = 0;
        p.format = -3;
        p.softInputMode = 1;
        p.flags = NOT_FOCUSABLE_FLAGS;
        p.type = 2;
        p.token = null;
        return p;
    }
}
