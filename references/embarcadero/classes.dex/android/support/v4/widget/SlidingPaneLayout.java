package android.support.v4.widget;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.PorterDuff.Mode;
import android.graphics.PorterDuffColorFilter;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;
import android.os.Build.VERSION;
import android.os.Parcel;
import android.os.Parcelable;
import android.os.Parcelable.Creator;
import android.support.v4.view.AccessibilityDelegateCompat;
import android.support.v4.view.MotionEventCompat;
import android.support.v4.view.ViewCompat;
import android.support.v4.view.accessibility.AccessibilityNodeInfoCompat;
import android.support.v4.widget.ViewDragHelper.Callback;
import android.util.AttributeSet;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.BaseSavedState;
import android.view.View.MeasureSpec;
import android.view.ViewConfiguration;
import android.view.ViewGroup;
import android.view.ViewGroup.MarginLayoutParams;
import android.view.ViewParent;
import android.view.accessibility.AccessibilityEvent;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;

public class SlidingPaneLayout extends ViewGroup {
    private static final int DEFAULT_FADE_COLOR = -858993460;
    private static final int DEFAULT_OVERHANG_SIZE = 32;
    static final SlidingPanelLayoutImpl IMPL;
    private static final int MIN_FLING_VELOCITY = 400;
    private static final String TAG = "SlidingPaneLayout";
    private boolean mCanSlide;
    private int mCoveredFadeColor;
    private final ViewDragHelper mDragHelper;
    private boolean mFirstLayout;
    private float mInitialMotionX;
    private float mInitialMotionY;
    private boolean mIsUnableToDrag;
    private final int mOverhangSize;
    private PanelSlideListener mPanelSlideListener;
    private int mParallaxBy;
    private float mParallaxOffset;
    private final ArrayList<DisableLayerRunnable> mPostedRunnables;
    private boolean mPreservedOpenState;
    private Drawable mShadowDrawable;
    private float mSlideOffset;
    private int mSlideRange;
    private View mSlideableView;
    private int mSliderFadeColor;
    private final Rect mTmpRect;

    private class DisableLayerRunnable implements Runnable {
        final View mChildView;

        DisableLayerRunnable(View childView) {
            this.mChildView = childView;
        }

        public void run() {
            if (this.mChildView.getParent() == SlidingPaneLayout.this) {
                ViewCompat.setLayerType(this.mChildView, 0, null);
                SlidingPaneLayout.this.invalidateChildRegion(this.mChildView);
            }
            SlidingPaneLayout.this.mPostedRunnables.remove(this);
        }
    }

    public static class LayoutParams extends MarginLayoutParams {
        private static final int[] ATTRS;
        Paint dimPaint;
        boolean dimWhenOffset;
        boolean slideable;
        public float weight;

        static {
            ATTRS = new int[]{16843137};
        }

        public LayoutParams() {
            super(-1, -1);
            this.weight = 0.0f;
        }

        public LayoutParams(int width, int height) {
            super(width, height);
            this.weight = 0.0f;
        }

        public LayoutParams(android.view.ViewGroup.LayoutParams source) {
            super(source);
            this.weight = 0.0f;
        }

        public LayoutParams(MarginLayoutParams source) {
            super(source);
            this.weight = 0.0f;
        }

        public LayoutParams(LayoutParams source) {
            super(source);
            this.weight = 0.0f;
            this.weight = source.weight;
        }

        public LayoutParams(Context c, AttributeSet attrs) {
            super(c, attrs);
            this.weight = 0.0f;
            TypedArray a = c.obtainStyledAttributes(attrs, ATTRS);
            this.weight = a.getFloat(0, 0.0f);
            a.recycle();
        }
    }

    public interface PanelSlideListener {
        void onPanelClosed(View view);

        void onPanelOpened(View view);

        void onPanelSlide(View view, float f);
    }

    static class SavedState extends BaseSavedState {
        public static final Creator<SavedState> CREATOR;
        boolean isOpen;

        SavedState(Parcelable superState) {
            super(superState);
        }

        private SavedState(Parcel in) {
            super(in);
            this.isOpen = in.readInt() != 0;
        }

        public void writeToParcel(Parcel out, int flags) {
            super.writeToParcel(out, flags);
            out.writeInt(this.isOpen ? 1 : 0);
        }

        static {
            CREATOR = new Creator<SavedState>() {
                public SavedState createFromParcel(Parcel in) {
                    return new SavedState(null);
                }

                public SavedState[] newArray(int size) {
                    return new SavedState[size];
                }
            };
        }
    }

    interface SlidingPanelLayoutImpl {
        void invalidateChildRegion(SlidingPaneLayout slidingPaneLayout, View view);
    }

    class AccessibilityDelegate extends AccessibilityDelegateCompat {
        private final Rect mTmpRect;

        AccessibilityDelegate() {
            this.mTmpRect = new Rect();
        }

        public void onInitializeAccessibilityNodeInfo(View host, AccessibilityNodeInfoCompat info) {
            AccessibilityNodeInfoCompat superNode = AccessibilityNodeInfoCompat.obtain(info);
            super.onInitializeAccessibilityNodeInfo(host, superNode);
            copyNodeInfoNoChildren(info, superNode);
            superNode.recycle();
            info.setClassName(SlidingPaneLayout.class.getName());
            info.setSource(host);
            ViewParent parent = ViewCompat.getParentForAccessibility(host);
            if (parent instanceof View) {
                info.setParent((View) parent);
            }
            int childCount = SlidingPaneLayout.this.getChildCount();
            for (int i = 0; i < childCount; i++) {
                View child = SlidingPaneLayout.this.getChildAt(i);
                if (!filter(child) && child.getVisibility() == 0) {
                    ViewCompat.setImportantForAccessibility(child, 1);
                    info.addChild(child);
                }
            }
        }

        public void onInitializeAccessibilityEvent(View host, AccessibilityEvent event) {
            super.onInitializeAccessibilityEvent(host, event);
            event.setClassName(SlidingPaneLayout.class.getName());
        }

        public boolean onRequestSendAccessibilityEvent(ViewGroup host, View child, AccessibilityEvent event) {
            if (filter(child)) {
                return false;
            }
            return super.onRequestSendAccessibilityEvent(host, child, event);
        }

        public boolean filter(View child) {
            return SlidingPaneLayout.this.isDimmed(child);
        }

        private void copyNodeInfoNoChildren(AccessibilityNodeInfoCompat dest, AccessibilityNodeInfoCompat src) {
            Rect rect = this.mTmpRect;
            src.getBoundsInParent(rect);
            dest.setBoundsInParent(rect);
            src.getBoundsInScreen(rect);
            dest.setBoundsInScreen(rect);
            dest.setVisibleToUser(src.isVisibleToUser());
            dest.setPackageName(src.getPackageName());
            dest.setClassName(src.getClassName());
            dest.setContentDescription(src.getContentDescription());
            dest.setEnabled(src.isEnabled());
            dest.setClickable(src.isClickable());
            dest.setFocusable(src.isFocusable());
            dest.setFocused(src.isFocused());
            dest.setAccessibilityFocused(src.isAccessibilityFocused());
            dest.setSelected(src.isSelected());
            dest.setLongClickable(src.isLongClickable());
            dest.addAction(src.getActions());
            dest.setMovementGranularities(src.getMovementGranularities());
        }
    }

    private class DragHelperCallback extends Callback {
        private DragHelperCallback() {
        }

        public boolean tryCaptureView(View child, int pointerId) {
            if (SlidingPaneLayout.this.mIsUnableToDrag) {
                return false;
            }
            return ((LayoutParams) child.getLayoutParams()).slideable;
        }

        public void onViewDragStateChanged(int state) {
            if (SlidingPaneLayout.this.mDragHelper.getViewDragState() != 0) {
                return;
            }
            if (SlidingPaneLayout.this.mSlideOffset == 0.0f) {
                SlidingPaneLayout.this.updateObscuredViewsVisibility(SlidingPaneLayout.this.mSlideableView);
                SlidingPaneLayout.this.dispatchOnPanelClosed(SlidingPaneLayout.this.mSlideableView);
                SlidingPaneLayout.this.mPreservedOpenState = false;
                return;
            }
            SlidingPaneLayout.this.dispatchOnPanelOpened(SlidingPaneLayout.this.mSlideableView);
            SlidingPaneLayout.this.mPreservedOpenState = true;
        }

        public void onViewCaptured(View capturedChild, int activePointerId) {
            SlidingPaneLayout.this.setAllChildrenVisible();
        }

        public void onViewPositionChanged(View changedView, int left, int top, int dx, int dy) {
            SlidingPaneLayout.this.onPanelDragged(left);
            SlidingPaneLayout.this.invalidate();
        }

        public void onViewReleased(View releasedChild, float xvel, float yvel) {
            int left = SlidingPaneLayout.this.getPaddingLeft() + ((LayoutParams) releasedChild.getLayoutParams()).leftMargin;
            if (xvel > 0.0f || (xvel == 0.0f && SlidingPaneLayout.this.mSlideOffset > 0.5f)) {
                left += SlidingPaneLayout.this.mSlideRange;
            }
            SlidingPaneLayout.this.mDragHelper.settleCapturedViewAt(left, releasedChild.getTop());
            SlidingPaneLayout.this.invalidate();
        }

        public int getViewHorizontalDragRange(View child) {
            return SlidingPaneLayout.this.mSlideRange;
        }

        public int clampViewPositionHorizontal(View child, int left, int dx) {
            int leftBound = SlidingPaneLayout.this.getPaddingLeft() + ((LayoutParams) SlidingPaneLayout.this.mSlideableView.getLayoutParams()).leftMargin;
            return Math.min(Math.max(left, leftBound), leftBound + SlidingPaneLayout.this.mSlideRange);
        }

        public void onEdgeDragStarted(int edgeFlags, int pointerId) {
            SlidingPaneLayout.this.mDragHelper.captureChildView(SlidingPaneLayout.this.mSlideableView, pointerId);
        }
    }

    public static class SimplePanelSlideListener implements PanelSlideListener {
        public void onPanelSlide(View panel, float slideOffset) {
        }

        public void onPanelOpened(View panel) {
        }

        public void onPanelClosed(View panel) {
        }
    }

    static class SlidingPanelLayoutImplBase implements SlidingPanelLayoutImpl {
        SlidingPanelLayoutImplBase() {
        }

        public void invalidateChildRegion(SlidingPaneLayout parent, View child) {
            ViewCompat.postInvalidateOnAnimation(parent, child.getLeft(), child.getTop(), child.getRight(), child.getBottom());
        }
    }

    static class SlidingPanelLayoutImplJB extends SlidingPanelLayoutImplBase {
        private Method mGetDisplayList;
        private Field mRecreateDisplayList;

        SlidingPanelLayoutImplJB() {
            try {
                this.mGetDisplayList = View.class.getDeclaredMethod("getDisplayList", (Class[]) null);
            } catch (NoSuchMethodException e) {
                Log.e(SlidingPaneLayout.TAG, "Couldn't fetch getDisplayList method; dimming won't work right.", e);
            }
            try {
                this.mRecreateDisplayList = View.class.getDeclaredField("mRecreateDisplayList");
                this.mRecreateDisplayList.setAccessible(true);
            } catch (NoSuchFieldException e2) {
                Log.e(SlidingPaneLayout.TAG, "Couldn't fetch mRecreateDisplayList field; dimming will be slow.", e2);
            }
        }

        public void invalidateChildRegion(SlidingPaneLayout parent, View child) {
            if (this.mGetDisplayList == null || this.mRecreateDisplayList == null) {
                child.invalidate();
                return;
            }
            try {
                this.mRecreateDisplayList.setBoolean(child, true);
                this.mGetDisplayList.invoke(child, (Object[]) null);
            } catch (Exception e) {
                Log.e(SlidingPaneLayout.TAG, "Error refreshing display list state", e);
            }
            super.invalidateChildRegion(parent, child);
        }
    }

    static class SlidingPanelLayoutImplJBMR1 extends SlidingPanelLayoutImplBase {
        SlidingPanelLayoutImplJBMR1() {
        }

        public void invalidateChildRegion(SlidingPaneLayout parent, View child) {
            ViewCompat.setLayerPaint(child, ((LayoutParams) child.getLayoutParams()).dimPaint);
        }
    }

    static {
        int deviceVersion = VERSION.SDK_INT;
        if (deviceVersion >= 17) {
            IMPL = new SlidingPanelLayoutImplJBMR1();
        } else if (deviceVersion >= 16) {
            IMPL = new SlidingPanelLayoutImplJB();
        } else {
            IMPL = new SlidingPanelLayoutImplBase();
        }
    }

    public SlidingPaneLayout(Context context) {
        this(context, null);
    }

    public SlidingPaneLayout(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public SlidingPaneLayout(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        this.mSliderFadeColor = DEFAULT_FADE_COLOR;
        this.mFirstLayout = true;
        this.mTmpRect = new Rect();
        this.mPostedRunnables = new ArrayList();
        float density = context.getResources().getDisplayMetrics().density;
        this.mOverhangSize = (int) ((32.0f * density) + 0.5f);
        ViewConfiguration viewConfig = ViewConfiguration.get(context);
        setWillNotDraw(false);
        ViewCompat.setAccessibilityDelegate(this, new AccessibilityDelegate());
        ViewCompat.setImportantForAccessibility(this, 1);
        this.mDragHelper = ViewDragHelper.create(this, 0.5f, new DragHelperCallback());
        this.mDragHelper.setEdgeTrackingEnabled(1);
        this.mDragHelper.setMinVelocity(400.0f * density);
    }

    public void setParallaxDistance(int parallaxBy) {
        this.mParallaxBy = parallaxBy;
        requestLayout();
    }

    public int getParallaxDistance() {
        return this.mParallaxBy;
    }

    public void setSliderFadeColor(int color) {
        this.mSliderFadeColor = color;
    }

    public int getSliderFadeColor() {
        return this.mSliderFadeColor;
    }

    public void setCoveredFadeColor(int color) {
        this.mCoveredFadeColor = color;
    }

    public int getCoveredFadeColor() {
        return this.mCoveredFadeColor;
    }

    public void setPanelSlideListener(PanelSlideListener listener) {
        this.mPanelSlideListener = listener;
    }

    void dispatchOnPanelSlide(View panel) {
        if (this.mPanelSlideListener != null) {
            this.mPanelSlideListener.onPanelSlide(panel, this.mSlideOffset);
        }
    }

    void dispatchOnPanelOpened(View panel) {
        if (this.mPanelSlideListener != null) {
            this.mPanelSlideListener.onPanelOpened(panel);
        }
        sendAccessibilityEvent(DEFAULT_OVERHANG_SIZE);
    }

    void dispatchOnPanelClosed(View panel) {
        if (this.mPanelSlideListener != null) {
            this.mPanelSlideListener.onPanelClosed(panel);
        }
        sendAccessibilityEvent(DEFAULT_OVERHANG_SIZE);
    }

    void updateObscuredViewsVisibility(View panel) {
        int left;
        int leftBound = getPaddingLeft();
        int rightBound = getWidth() - getPaddingRight();
        int topBound = getPaddingTop();
        int bottomBound = getHeight() - getPaddingBottom();
        int bottom;
        int top;
        int right;
        if (panel == null || !viewIsOpaque(panel)) {
            bottom = 0;
            top = 0;
            right = 0;
            left = 0;
        } else {
            left = panel.getLeft();
            right = panel.getRight();
            top = panel.getTop();
            bottom = panel.getBottom();
        }
        int i = 0;
        int childCount = getChildCount();
        while (i < childCount) {
            View child = getChildAt(i);
            if (child != panel) {
                int vis;
                int clampedChildLeft = Math.max(leftBound, child.getLeft());
                int clampedChildTop = Math.max(topBound, child.getTop());
                int clampedChildRight = Math.min(rightBound, child.getRight());
                int clampedChildBottom = Math.min(bottomBound, child.getBottom());
                if (clampedChildLeft < left || clampedChildTop < top || clampedChildRight > right || clampedChildBottom > bottom) {
                    vis = 0;
                } else {
                    vis = 4;
                }
                child.setVisibility(vis);
                i++;
            } else {
                return;
            }
        }
    }

    void setAllChildrenVisible() {
        int childCount = getChildCount();
        for (int i = 0; i < childCount; i++) {
            View child = getChildAt(i);
            if (child.getVisibility() == 4) {
                child.setVisibility(0);
            }
        }
    }

    private static boolean viewIsOpaque(View v) {
        if (ViewCompat.isOpaque(v)) {
            return true;
        }
        if (VERSION.SDK_INT >= 18) {
            return false;
        }
        Drawable bg = v.getBackground();
        if (bg == null) {
            return false;
        }
        if (bg.getOpacity() != -1) {
            return false;
        }
        return true;
    }

    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        this.mFirstLayout = true;
    }

    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        this.mFirstLayout = true;
        int count = this.mPostedRunnables.size();
        for (int i = 0; i < count; i++) {
            ((DisableLayerRunnable) this.mPostedRunnables.get(i)).run();
        }
        this.mPostedRunnables.clear();
    }

    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        int i;
        int i2;
        int childWidthSpec;
        int childHeightSpec;
        int widthMode = MeasureSpec.getMode(widthMeasureSpec);
        int widthSize = MeasureSpec.getSize(widthMeasureSpec);
        int heightMode = MeasureSpec.getMode(heightMeasureSpec);
        int heightSize = MeasureSpec.getSize(heightMeasureSpec);
        if (widthMode != 1073741824) {
            if (!isInEditMode()) {
                throw new IllegalStateException("Width must have an exact value or MATCH_PARENT");
            } else if (widthMode != Integer.MIN_VALUE) {
                if (widthMode == 0) {
                    widthSize = 300;
                }
            }
        } else if (heightMode == 0) {
            if (!isInEditMode()) {
                throw new IllegalStateException("Height must not be UNSPECIFIED");
            } else if (heightMode == 0) {
                heightMode = ExploreByTouchHelper.INVALID_ID;
                heightSize = 300;
            }
        }
        int layoutHeight = 0;
        int maxLayoutHeight = -1;
        switch (heightMode) {
            case ExploreByTouchHelper.INVALID_ID /*-2147483648*/:
                maxLayoutHeight = (heightSize - getPaddingTop()) - getPaddingBottom();
                break;
            case 1073741824:
                maxLayoutHeight = (heightSize - getPaddingTop()) - getPaddingBottom();
                layoutHeight = maxLayoutHeight;
                break;
        }
        float weightSum = 0.0f;
        boolean canSlide = false;
        int widthRemaining = (widthSize - getPaddingLeft()) - getPaddingRight();
        int childCount = getChildCount();
        if (childCount > 2) {
            Log.e(TAG, "onMeasure: More than two child views are not supported.");
        }
        this.mSlideableView = null;
        for (i = 0; i < childCount; i++) {
            View child = getChildAt(i);
            LayoutParams lp = (LayoutParams) child.getLayoutParams();
            if (child.getVisibility() == 8) {
                lp.dimWhenOffset = false;
            } else {
                if (lp.weight > 0.0f) {
                    weightSum += lp.weight;
                    if (lp.width == 0) {
                    }
                }
                int horizontalMargin = lp.leftMargin + lp.rightMargin;
                i2 = lp.width;
                if (r0 == -2) {
                    childWidthSpec = MeasureSpec.makeMeasureSpec(widthSize - horizontalMargin, ExploreByTouchHelper.INVALID_ID);
                } else {
                    i2 = lp.width;
                    if (r0 == -1) {
                        childWidthSpec = MeasureSpec.makeMeasureSpec(widthSize - horizontalMargin, 1073741824);
                    } else {
                        childWidthSpec = MeasureSpec.makeMeasureSpec(lp.width, 1073741824);
                    }
                }
                i2 = lp.height;
                if (r0 == -2) {
                    childHeightSpec = MeasureSpec.makeMeasureSpec(maxLayoutHeight, ExploreByTouchHelper.INVALID_ID);
                } else {
                    i2 = lp.height;
                    if (r0 == -1) {
                        childHeightSpec = MeasureSpec.makeMeasureSpec(maxLayoutHeight, 1073741824);
                    } else {
                        childHeightSpec = MeasureSpec.makeMeasureSpec(lp.height, 1073741824);
                    }
                }
                child.measure(childWidthSpec, childHeightSpec);
                int childWidth = child.getMeasuredWidth();
                int childHeight = child.getMeasuredHeight();
                if (heightMode == Integer.MIN_VALUE && childHeight > layoutHeight) {
                    layoutHeight = Math.min(childHeight, maxLayoutHeight);
                }
                widthRemaining -= childWidth;
                boolean z = widthRemaining < 0;
                lp.slideable = z;
                canSlide |= z;
                if (lp.slideable) {
                    this.mSlideableView = child;
                }
            }
        }
        if (canSlide || weightSum > 0.0f) {
            int fixedPanelWidthLimit = widthSize - this.mOverhangSize;
            for (i = 0; i < childCount; i++) {
                child = getChildAt(i);
                if (child.getVisibility() != 8) {
                    lp = (LayoutParams) child.getLayoutParams();
                    if (child.getVisibility() != 8) {
                        boolean skippedFirstPass;
                        int measuredWidth;
                        View view;
                        int newWidth;
                        if (lp.width == 0) {
                            if (lp.weight > 0.0f) {
                                skippedFirstPass = true;
                                measuredWidth = skippedFirstPass ? 0 : child.getMeasuredWidth();
                                if (canSlide) {
                                    view = this.mSlideableView;
                                    if (child != r0) {
                                        if (lp.width >= 0) {
                                            if (measuredWidth <= fixedPanelWidthLimit) {
                                                if (lp.weight <= 0.0f) {
                                                }
                                            }
                                            if (skippedFirstPass) {
                                                childHeightSpec = MeasureSpec.makeMeasureSpec(child.getMeasuredHeight(), 1073741824);
                                            } else {
                                                i2 = lp.height;
                                                if (r0 != -2) {
                                                    childHeightSpec = MeasureSpec.makeMeasureSpec(maxLayoutHeight, ExploreByTouchHelper.INVALID_ID);
                                                } else {
                                                    i2 = lp.height;
                                                    if (r0 != -1) {
                                                        childHeightSpec = MeasureSpec.makeMeasureSpec(maxLayoutHeight, 1073741824);
                                                    } else {
                                                        childHeightSpec = MeasureSpec.makeMeasureSpec(lp.height, 1073741824);
                                                    }
                                                }
                                            }
                                            child.measure(MeasureSpec.makeMeasureSpec(fixedPanelWidthLimit, 1073741824), childHeightSpec);
                                        }
                                    }
                                }
                                if (lp.weight <= 0.0f) {
                                    if (lp.width != 0) {
                                        i2 = lp.height;
                                        if (r0 != -2) {
                                            childHeightSpec = MeasureSpec.makeMeasureSpec(maxLayoutHeight, ExploreByTouchHelper.INVALID_ID);
                                        } else {
                                            i2 = lp.height;
                                            if (r0 != -1) {
                                                childHeightSpec = MeasureSpec.makeMeasureSpec(maxLayoutHeight, 1073741824);
                                            } else {
                                                childHeightSpec = MeasureSpec.makeMeasureSpec(lp.height, 1073741824);
                                            }
                                        }
                                    } else {
                                        childHeightSpec = MeasureSpec.makeMeasureSpec(child.getMeasuredHeight(), 1073741824);
                                    }
                                    if (canSlide) {
                                        child.measure(MeasureSpec.makeMeasureSpec(measuredWidth + ((int) ((lp.weight * ((float) Math.max(0, widthRemaining))) / weightSum)), 1073741824), childHeightSpec);
                                    } else {
                                        newWidth = widthSize - (lp.leftMargin + lp.rightMargin);
                                        childWidthSpec = MeasureSpec.makeMeasureSpec(newWidth, 1073741824);
                                        if (measuredWidth != newWidth) {
                                            child.measure(childWidthSpec, childHeightSpec);
                                        }
                                    }
                                }
                            }
                        }
                        skippedFirstPass = false;
                        if (skippedFirstPass) {
                        }
                        if (canSlide) {
                            view = this.mSlideableView;
                            if (child != r0) {
                                if (lp.width >= 0) {
                                    if (measuredWidth <= fixedPanelWidthLimit) {
                                        if (lp.weight <= 0.0f) {
                                        }
                                    }
                                    if (skippedFirstPass) {
                                        childHeightSpec = MeasureSpec.makeMeasureSpec(child.getMeasuredHeight(), 1073741824);
                                    } else {
                                        i2 = lp.height;
                                        if (r0 != -2) {
                                            i2 = lp.height;
                                            if (r0 != -1) {
                                                childHeightSpec = MeasureSpec.makeMeasureSpec(lp.height, 1073741824);
                                            } else {
                                                childHeightSpec = MeasureSpec.makeMeasureSpec(maxLayoutHeight, 1073741824);
                                            }
                                        } else {
                                            childHeightSpec = MeasureSpec.makeMeasureSpec(maxLayoutHeight, ExploreByTouchHelper.INVALID_ID);
                                        }
                                    }
                                    child.measure(MeasureSpec.makeMeasureSpec(fixedPanelWidthLimit, 1073741824), childHeightSpec);
                                }
                            }
                        }
                        if (lp.weight <= 0.0f) {
                            if (lp.width != 0) {
                                childHeightSpec = MeasureSpec.makeMeasureSpec(child.getMeasuredHeight(), 1073741824);
                            } else {
                                i2 = lp.height;
                                if (r0 != -2) {
                                    i2 = lp.height;
                                    if (r0 != -1) {
                                        childHeightSpec = MeasureSpec.makeMeasureSpec(lp.height, 1073741824);
                                    } else {
                                        childHeightSpec = MeasureSpec.makeMeasureSpec(maxLayoutHeight, 1073741824);
                                    }
                                } else {
                                    childHeightSpec = MeasureSpec.makeMeasureSpec(maxLayoutHeight, ExploreByTouchHelper.INVALID_ID);
                                }
                            }
                            if (canSlide) {
                                child.measure(MeasureSpec.makeMeasureSpec(measuredWidth + ((int) ((lp.weight * ((float) Math.max(0, widthRemaining))) / weightSum)), 1073741824), childHeightSpec);
                            } else {
                                newWidth = widthSize - (lp.leftMargin + lp.rightMargin);
                                childWidthSpec = MeasureSpec.makeMeasureSpec(newWidth, 1073741824);
                                if (measuredWidth != newWidth) {
                                    child.measure(childWidthSpec, childHeightSpec);
                                }
                            }
                        }
                    }
                }
            }
        }
        setMeasuredDimension(widthSize, layoutHeight);
        this.mCanSlide = canSlide;
        if (this.mDragHelper.getViewDragState() != 0 && !canSlide) {
            this.mDragHelper.abort();
        }
    }

    protected void onLayout(boolean changed, int l, int t, int r, int b) {
        int i;
        int width = r - l;
        int paddingLeft = getPaddingLeft();
        int paddingRight = getPaddingRight();
        int paddingTop = getPaddingTop();
        int childCount = getChildCount();
        int xStart = paddingLeft;
        int nextXStart = xStart;
        if (this.mFirstLayout) {
            float f = (this.mCanSlide && this.mPreservedOpenState) ? 1.0f : 0.0f;
            this.mSlideOffset = f;
        }
        for (i = 0; i < childCount; i++) {
            View child = getChildAt(i);
            if (child.getVisibility() != 8) {
                LayoutParams lp = (LayoutParams) child.getLayoutParams();
                int childWidth = child.getMeasuredWidth();
                int offset = 0;
                if (lp.slideable) {
                    int range = (Math.min(nextXStart, (width - paddingRight) - this.mOverhangSize) - xStart) - (lp.leftMargin + lp.rightMargin);
                    this.mSlideRange = range;
                    lp.dimWhenOffset = ((lp.leftMargin + xStart) + range) + (childWidth / 2) > width - paddingRight;
                    int pos = (int) (((float) range) * this.mSlideOffset);
                    xStart += lp.leftMargin + pos;
                    this.mSlideOffset = ((float) pos) / ((float) this.mSlideRange);
                } else if (!this.mCanSlide || this.mParallaxBy == 0) {
                    xStart = nextXStart;
                } else {
                    offset = (int) ((1.0f - this.mSlideOffset) * ((float) this.mParallaxBy));
                    xStart = nextXStart;
                }
                int childLeft = xStart - offset;
                child.layout(childLeft, paddingTop, childLeft + childWidth, paddingTop + child.getMeasuredHeight());
                nextXStart += child.getWidth();
            }
        }
        if (this.mFirstLayout) {
            if (this.mCanSlide) {
                if (this.mParallaxBy != 0) {
                    parallaxOtherViews(this.mSlideOffset);
                }
                if (((LayoutParams) this.mSlideableView.getLayoutParams()).dimWhenOffset) {
                    dimChildView(this.mSlideableView, this.mSlideOffset, this.mSliderFadeColor);
                }
            } else {
                for (i = 0; i < childCount; i++) {
                    dimChildView(getChildAt(i), 0.0f, this.mSliderFadeColor);
                }
            }
            updateObscuredViewsVisibility(this.mSlideableView);
        }
        this.mFirstLayout = false;
    }

    protected void onSizeChanged(int w, int h, int oldw, int oldh) {
        super.onSizeChanged(w, h, oldw, oldh);
        if (w != oldw) {
            this.mFirstLayout = true;
        }
    }

    public void requestChildFocus(View child, View focused) {
        super.requestChildFocus(child, focused);
        if (!isInTouchMode() && !this.mCanSlide) {
            this.mPreservedOpenState = child == this.mSlideableView;
        }
    }

    public boolean onInterceptTouchEvent(MotionEvent ev) {
        int action = MotionEventCompat.getActionMasked(ev);
        if (!this.mCanSlide && action == 0 && getChildCount() > 1) {
            View secondChild = getChildAt(1);
            if (secondChild != null) {
                this.mPreservedOpenState = !this.mDragHelper.isViewUnder(secondChild, (int) ev.getX(), (int) ev.getY());
            }
        }
        if (!this.mCanSlide || (this.mIsUnableToDrag && action != 0)) {
            this.mDragHelper.cancel();
            return super.onInterceptTouchEvent(ev);
        } else if (action == 3 || action == 1) {
            this.mDragHelper.cancel();
            return false;
        } else {
            boolean interceptTap = false;
            float x;
            float y;
            switch (action) {
                case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                    this.mIsUnableToDrag = false;
                    x = ev.getX();
                    y = ev.getY();
                    this.mInitialMotionX = x;
                    this.mInitialMotionY = y;
                    if (this.mDragHelper.isViewUnder(this.mSlideableView, (int) x, (int) y) && isDimmed(this.mSlideableView)) {
                        interceptTap = true;
                        break;
                    }
                case DetectedActivity.ON_FOOT /*2*/:
                    x = ev.getX();
                    y = ev.getY();
                    float adx = Math.abs(x - this.mInitialMotionX);
                    float ady = Math.abs(y - this.mInitialMotionY);
                    if (adx > ((float) this.mDragHelper.getTouchSlop()) && ady > adx) {
                        this.mDragHelper.cancel();
                        this.mIsUnableToDrag = true;
                        return false;
                    }
            }
            if (this.mDragHelper.shouldInterceptTouchEvent(ev) || interceptTap) {
                return true;
            }
            return false;
        }
    }

    public boolean onTouchEvent(MotionEvent ev) {
        if (!this.mCanSlide) {
            return super.onTouchEvent(ev);
        }
        this.mDragHelper.processTouchEvent(ev);
        float x;
        float y;
        switch (ev.getAction() & MotionEventCompat.ACTION_MASK) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                x = ev.getX();
                y = ev.getY();
                this.mInitialMotionX = x;
                this.mInitialMotionY = y;
                return true;
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                if (!isDimmed(this.mSlideableView)) {
                    return true;
                }
                x = ev.getX();
                y = ev.getY();
                float dx = x - this.mInitialMotionX;
                float dy = y - this.mInitialMotionY;
                int slop = this.mDragHelper.getTouchSlop();
                if ((dx * dx) + (dy * dy) >= ((float) (slop * slop)) || !this.mDragHelper.isViewUnder(this.mSlideableView, (int) x, (int) y)) {
                    return true;
                }
                closePane(this.mSlideableView, 0);
                return true;
            default:
                return true;
        }
    }

    private boolean closePane(View pane, int initialVelocity) {
        if (!this.mFirstLayout && !smoothSlideTo(0.0f, initialVelocity)) {
            return false;
        }
        this.mPreservedOpenState = false;
        return true;
    }

    private boolean openPane(View pane, int initialVelocity) {
        if (!this.mFirstLayout && !smoothSlideTo(1.0f, initialVelocity)) {
            return false;
        }
        this.mPreservedOpenState = true;
        return true;
    }

    @Deprecated
    public void smoothSlideOpen() {
        openPane();
    }

    public boolean openPane() {
        return openPane(this.mSlideableView, 0);
    }

    @Deprecated
    public void smoothSlideClosed() {
        closePane();
    }

    public boolean closePane() {
        return closePane(this.mSlideableView, 0);
    }

    public boolean isOpen() {
        return !this.mCanSlide || this.mSlideOffset == 1.0f;
    }

    @Deprecated
    public boolean canSlide() {
        return this.mCanSlide;
    }

    public boolean isSlideable() {
        return this.mCanSlide;
    }

    private void onPanelDragged(int newLeft) {
        if (this.mSlideableView == null) {
            this.mSlideOffset = 0.0f;
            return;
        }
        LayoutParams lp = (LayoutParams) this.mSlideableView.getLayoutParams();
        this.mSlideOffset = ((float) (newLeft - (getPaddingLeft() + lp.leftMargin))) / ((float) this.mSlideRange);
        if (this.mParallaxBy != 0) {
            parallaxOtherViews(this.mSlideOffset);
        }
        if (lp.dimWhenOffset) {
            dimChildView(this.mSlideableView, this.mSlideOffset, this.mSliderFadeColor);
        }
        dispatchOnPanelSlide(this.mSlideableView);
    }

    private void dimChildView(View v, float mag, int fadeColor) {
        LayoutParams lp = (LayoutParams) v.getLayoutParams();
        if (mag > 0.0f && fadeColor != 0) {
            int color = (((int) (((float) ((ViewCompat.MEASURED_STATE_MASK & fadeColor) >>> 24)) * mag)) << 24) | (ViewCompat.MEASURED_SIZE_MASK & fadeColor);
            if (lp.dimPaint == null) {
                lp.dimPaint = new Paint();
            }
            lp.dimPaint.setColorFilter(new PorterDuffColorFilter(color, Mode.SRC_OVER));
            if (ViewCompat.getLayerType(v) != 2) {
                ViewCompat.setLayerType(v, 2, lp.dimPaint);
            }
            invalidateChildRegion(v);
        } else if (ViewCompat.getLayerType(v) != 0) {
            if (lp.dimPaint != null) {
                lp.dimPaint.setColorFilter(null);
            }
            DisableLayerRunnable dlr = new DisableLayerRunnable(v);
            this.mPostedRunnables.add(dlr);
            ViewCompat.postOnAnimation(this, dlr);
        }
    }

    protected boolean drawChild(Canvas canvas, View child, long drawingTime) {
        boolean result;
        LayoutParams lp = (LayoutParams) child.getLayoutParams();
        int save = canvas.save(2);
        if (!(!this.mCanSlide || lp.slideable || this.mSlideableView == null)) {
            canvas.getClipBounds(this.mTmpRect);
            this.mTmpRect.right = Math.min(this.mTmpRect.right, this.mSlideableView.getLeft());
            canvas.clipRect(this.mTmpRect);
        }
        if (VERSION.SDK_INT >= 11) {
            result = super.drawChild(canvas, child, drawingTime);
        } else if (!lp.dimWhenOffset || this.mSlideOffset <= 0.0f) {
            if (child.isDrawingCacheEnabled()) {
                child.setDrawingCacheEnabled(false);
            }
            result = super.drawChild(canvas, child, drawingTime);
        } else {
            if (!child.isDrawingCacheEnabled()) {
                child.setDrawingCacheEnabled(true);
            }
            Bitmap cache = child.getDrawingCache();
            if (cache != null) {
                canvas.drawBitmap(cache, (float) child.getLeft(), (float) child.getTop(), lp.dimPaint);
                result = false;
            } else {
                Log.e(TAG, "drawChild: child view " + child + " returned null drawing cache");
                result = super.drawChild(canvas, child, drawingTime);
            }
        }
        canvas.restoreToCount(save);
        return result;
    }

    private void invalidateChildRegion(View v) {
        IMPL.invalidateChildRegion(this, v);
    }

    boolean smoothSlideTo(float slideOffset, int velocity) {
        if (!this.mCanSlide) {
            return false;
        }
        if (!this.mDragHelper.smoothSlideViewTo(this.mSlideableView, (int) (((float) (getPaddingLeft() + ((LayoutParams) this.mSlideableView.getLayoutParams()).leftMargin)) + (((float) this.mSlideRange) * slideOffset)), this.mSlideableView.getTop())) {
            return false;
        }
        setAllChildrenVisible();
        ViewCompat.postInvalidateOnAnimation(this);
        return true;
    }

    public void computeScroll() {
        if (!this.mDragHelper.continueSettling(true)) {
            return;
        }
        if (this.mCanSlide) {
            ViewCompat.postInvalidateOnAnimation(this);
        } else {
            this.mDragHelper.abort();
        }
    }

    public void setShadowDrawable(Drawable d) {
        this.mShadowDrawable = d;
    }

    public void setShadowResource(int resId) {
        setShadowDrawable(getResources().getDrawable(resId));
    }

    public void draw(Canvas c) {
        super.draw(c);
        View shadowView = getChildCount() > 1 ? getChildAt(1) : null;
        if (shadowView != null && this.mShadowDrawable != null) {
            int shadowWidth = this.mShadowDrawable.getIntrinsicWidth();
            int right = shadowView.getLeft();
            this.mShadowDrawable.setBounds(right - shadowWidth, shadowView.getTop(), right, shadowView.getBottom());
            this.mShadowDrawable.draw(c);
        }
    }

    private void parallaxOtherViews(float slideOffset) {
        LayoutParams slideLp = (LayoutParams) this.mSlideableView.getLayoutParams();
        boolean dimViews = slideLp.dimWhenOffset && slideLp.leftMargin <= 0;
        int childCount = getChildCount();
        for (int i = 0; i < childCount; i++) {
            View v = getChildAt(i);
            if (v != this.mSlideableView) {
                int oldOffset = (int) ((1.0f - this.mParallaxOffset) * ((float) this.mParallaxBy));
                this.mParallaxOffset = slideOffset;
                v.offsetLeftAndRight(oldOffset - ((int) ((1.0f - slideOffset) * ((float) this.mParallaxBy))));
                if (dimViews) {
                    dimChildView(v, 1.0f - this.mParallaxOffset, this.mCoveredFadeColor);
                }
            }
        }
    }

    protected boolean canScroll(View v, boolean checkV, int dx, int x, int y) {
        if (v instanceof ViewGroup) {
            ViewGroup group = (ViewGroup) v;
            int scrollX = v.getScrollX();
            int scrollY = v.getScrollY();
            for (int i = group.getChildCount() - 1; i >= 0; i--) {
                View child = group.getChildAt(i);
                if (x + scrollX >= child.getLeft() && x + scrollX < child.getRight() && y + scrollY >= child.getTop() && y + scrollY < child.getBottom()) {
                    if (canScroll(child, true, dx, (x + scrollX) - child.getLeft(), (y + scrollY) - child.getTop())) {
                        return true;
                    }
                }
            }
        }
        return checkV && ViewCompat.canScrollHorizontally(v, -dx);
    }

    boolean isDimmed(View child) {
        if (child == null) {
            return false;
        }
        LayoutParams lp = (LayoutParams) child.getLayoutParams();
        if (this.mCanSlide && lp.dimWhenOffset && this.mSlideOffset > 0.0f) {
            return true;
        }
        return false;
    }

    protected android.view.ViewGroup.LayoutParams generateDefaultLayoutParams() {
        return new LayoutParams();
    }

    protected android.view.ViewGroup.LayoutParams generateLayoutParams(android.view.ViewGroup.LayoutParams p) {
        return p instanceof MarginLayoutParams ? new LayoutParams((MarginLayoutParams) p) : new LayoutParams(p);
    }

    protected boolean checkLayoutParams(android.view.ViewGroup.LayoutParams p) {
        return (p instanceof LayoutParams) && super.checkLayoutParams(p);
    }

    public android.view.ViewGroup.LayoutParams generateLayoutParams(AttributeSet attrs) {
        return new LayoutParams(getContext(), attrs);
    }

    protected Parcelable onSaveInstanceState() {
        SavedState ss = new SavedState(super.onSaveInstanceState());
        ss.isOpen = isSlideable() ? isOpen() : this.mPreservedOpenState;
        return ss;
    }

    protected void onRestoreInstanceState(Parcelable state) {
        SavedState ss = (SavedState) state;
        super.onRestoreInstanceState(ss.getSuperState());
        if (ss.isOpen) {
            openPane();
        } else {
            closePane();
        }
        this.mPreservedOpenState = ss.isOpen;
    }
}
