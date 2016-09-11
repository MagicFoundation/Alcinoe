package com.google.android.gms.internal;

import android.app.Activity;
import android.content.Context;
import android.os.Binder;
import android.os.Bundle;
import android.os.IBinder;
import android.view.Display;
import android.view.View;
import android.view.View.OnAttachStateChangeListener;
import android.view.ViewTreeObserver;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;
import java.lang.ref.WeakReference;

public class gd {
    protected fx HB;
    protected a HC;

    public static final class a {
        public IBinder HD;
        public int HE;
        public int bottom;
        public int gravity;
        public int left;
        public int right;
        public int top;

        private a(int i, IBinder iBinder) {
            this.HE = -1;
            this.left = 0;
            this.top = 0;
            this.right = 0;
            this.bottom = 0;
            this.gravity = i;
            this.HD = iBinder;
        }

        public Bundle fQ() {
            Bundle bundle = new Bundle();
            bundle.putInt("popupLocationInfo.gravity", this.gravity);
            bundle.putInt("popupLocationInfo.displayId", this.HE);
            bundle.putInt("popupLocationInfo.left", this.left);
            bundle.putInt("popupLocationInfo.top", this.top);
            bundle.putInt("popupLocationInfo.right", this.right);
            bundle.putInt("popupLocationInfo.bottom", this.bottom);
            return bundle;
        }
    }

    private static final class b extends gd implements OnAttachStateChangeListener, OnGlobalLayoutListener {
        private boolean GA;
        private WeakReference<View> HF;

        protected b(fx fxVar, int i) {
            super(i, null);
            this.GA = false;
        }

        private void g(View view) {
            int i = -1;
            if (fr.eO()) {
                Display display = view.getDisplay();
                if (display != null) {
                    i = display.getDisplayId();
                }
            }
            IBinder windowToken = view.getWindowToken();
            int[] iArr = new int[2];
            view.getLocationInWindow(iArr);
            int width = view.getWidth();
            int height = view.getHeight();
            this.HC.HE = i;
            this.HC.HD = windowToken;
            this.HC.left = iArr[0];
            this.HC.top = iArr[1];
            this.HC.right = iArr[0] + width;
            this.HC.bottom = iArr[1] + height;
            if (this.GA) {
                fN();
                this.GA = false;
            }
        }

        protected void aV(int i) {
            this.HC = new a(null, null);
        }

        public void f(View view) {
            View view2;
            Context context;
            this.HB.fH();
            if (this.HF != null) {
                view2 = (View) this.HF.get();
                context = this.HB.getContext();
                if (view2 == null && (context instanceof Activity)) {
                    view2 = ((Activity) context).getWindow().getDecorView();
                }
                if (view2 != null) {
                    view2.removeOnAttachStateChangeListener(this);
                    ViewTreeObserver viewTreeObserver = view2.getViewTreeObserver();
                    if (fr.eN()) {
                        viewTreeObserver.removeOnGlobalLayoutListener(this);
                    } else {
                        viewTreeObserver.removeGlobalOnLayoutListener(this);
                    }
                }
            }
            this.HF = null;
            context = this.HB.getContext();
            if (view == null && (context instanceof Activity)) {
                view2 = ((Activity) context).findViewById(16908290);
                if (view2 == null) {
                    view2 = ((Activity) context).getWindow().getDecorView();
                }
                fz.g("PopupManager", "You have not specified a View to use as content view for popups. Falling back to the Activity content view which may not work properly in future versions of the API. Use setViewForPopups() to set your content view.");
                view = view2;
            }
            if (view != null) {
                g(view);
                this.HF = new WeakReference(view);
                view.addOnAttachStateChangeListener(this);
                view.getViewTreeObserver().addOnGlobalLayoutListener(this);
                return;
            }
            fz.h("PopupManager", "No content view usable to display popups. Popups will not be displayed in response to this client's calls. Use setViewForPopups() to set your content view.");
        }

        public void fN() {
            if (this.HC.HD != null) {
                super.fN();
            } else {
                this.GA = this.HF != null;
            }
        }

        public void onGlobalLayout() {
            if (this.HF != null) {
                View view = (View) this.HF.get();
                if (view != null) {
                    g(view);
                }
            }
        }

        public void onViewAttachedToWindow(View v) {
            g(v);
        }

        public void onViewDetachedFromWindow(View v) {
            this.HB.fH();
            v.removeOnAttachStateChangeListener(this);
        }
    }

    private gd(fx fxVar, int i) {
        this.HB = fxVar;
        aV(i);
    }

    public static gd a(fx fxVar, int i) {
        return fr.eK() ? new b(fxVar, i) : new gd(fxVar, i);
    }

    protected void aV(int i) {
        this.HC = new a(new Binder(), null);
    }

    public void f(View view) {
    }

    public void fN() {
        this.HB.a(this.HC.HD, this.HC.fQ());
    }

    public Bundle fO() {
        return this.HC.fQ();
    }

    public IBinder fP() {
        return this.HC.HD;
    }

    public void setGravity(int gravity) {
        this.HC.gravity = gravity;
    }
}
