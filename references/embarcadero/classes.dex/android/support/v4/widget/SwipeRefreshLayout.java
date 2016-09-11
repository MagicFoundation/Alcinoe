package android.support.v4.widget;

import android.content.Context;
import android.content.res.Resources;
import android.content.res.TypedArray;
import android.graphics.Canvas;
import android.os.Build.VERSION;
import android.support.v4.view.ViewCompat;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.MeasureSpec;
import android.view.ViewConfiguration;
import android.view.ViewGroup;
import android.view.animation.AccelerateInterpolator;
import android.view.animation.Animation;
import android.view.animation.Animation.AnimationListener;
import android.view.animation.DecelerateInterpolator;
import android.view.animation.Transformation;
import android.widget.AbsListView;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.GroundOverlayOptions;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class SwipeRefreshLayout extends ViewGroup {
    private static final float ACCELERATE_INTERPOLATION_FACTOR = 1.5f;
    private static final float DECELERATE_INTERPOLATION_FACTOR = 2.0f;
    private static final int[] LAYOUT_ATTRS;
    private static final float MAX_SWIPE_DISTANCE_FACTOR = 0.6f;
    private static final float PROGRESS_BAR_HEIGHT = 4.0f;
    private static final int REFRESH_TRIGGER_DISTANCE = 120;
    private static final long RETURN_TO_ORIGINAL_POSITION_TIMEOUT = 300;
    private final AccelerateInterpolator mAccelerateInterpolator;
    private final Animation mAnimateToStartPosition;
    private final Runnable mCancel;
    private float mCurrPercentage;
    private int mCurrentTargetOffsetTop;
    private final DecelerateInterpolator mDecelerateInterpolator;
    private float mDistanceToTriggerSync;
    private MotionEvent mDownEvent;
    private int mFrom;
    private float mFromPercentage;
    private OnRefreshListener mListener;
    private int mMediumAnimationDuration;
    private int mOriginalOffsetTop;
    private float mPrevY;
    private SwipeProgressBar mProgressBar;
    private int mProgressBarHeight;
    private boolean mRefreshing;
    private final Runnable mReturnToStartPosition;
    private final AnimationListener mReturnToStartPositionListener;
    private boolean mReturningToStart;
    private final AnimationListener mShrinkAnimationListener;
    private Animation mShrinkTrigger;
    private View mTarget;
    private int mTouchSlop;

    private class BaseAnimationListener implements AnimationListener {
        private BaseAnimationListener() {
        }

        public void onAnimationStart(Animation animation) {
        }

        public void onAnimationEnd(Animation animation) {
        }

        public void onAnimationRepeat(Animation animation) {
        }
    }

    public interface OnRefreshListener {
        void onRefresh();
    }

    static {
        LAYOUT_ATTRS = new int[]{16842766};
    }

    public SwipeRefreshLayout(Context context) {
        this(context, null);
    }

    public SwipeRefreshLayout(Context context, AttributeSet attrs) {
        super(context, attrs);
        this.mRefreshing = false;
        this.mDistanceToTriggerSync = GroundOverlayOptions.NO_DIMENSION;
        this.mFromPercentage = 0.0f;
        this.mCurrPercentage = 0.0f;
        this.mAnimateToStartPosition = new Animation() {
            public void applyTransformation(float interpolatedTime, Transformation t) {
                int targetTop = 0;
                if (SwipeRefreshLayout.this.mFrom != SwipeRefreshLayout.this.mOriginalOffsetTop) {
                    targetTop = SwipeRefreshLayout.this.mFrom + ((int) (((float) (SwipeRefreshLayout.this.mOriginalOffsetTop - SwipeRefreshLayout.this.mFrom)) * interpolatedTime));
                }
                int offset = targetTop - SwipeRefreshLayout.this.mTarget.getTop();
                int currentTop = SwipeRefreshLayout.this.mTarget.getTop();
                if (offset + currentTop < 0) {
                    offset = 0 - currentTop;
                }
                SwipeRefreshLayout.this.setTargetOffsetTopAndBottom(offset);
            }
        };
        this.mShrinkTrigger = new Animation() {
            public void applyTransformation(float interpolatedTime, Transformation t) {
                SwipeRefreshLayout.this.mProgressBar.setTriggerPercentage(SwipeRefreshLayout.this.mFromPercentage + ((0.0f - SwipeRefreshLayout.this.mFromPercentage) * interpolatedTime));
            }
        };
        this.mReturnToStartPositionListener = new BaseAnimationListener() {
            public void onAnimationEnd(Animation animation) {
                SwipeRefreshLayout.this.mCurrentTargetOffsetTop = 0;
            }
        };
        this.mShrinkAnimationListener = new BaseAnimationListener() {
            public void onAnimationEnd(Animation animation) {
                SwipeRefreshLayout.this.mCurrPercentage = 0.0f;
            }
        };
        this.mReturnToStartPosition = new Runnable() {
            public void run() {
                SwipeRefreshLayout.this.mReturningToStart = true;
                SwipeRefreshLayout.this.animateOffsetToStartPosition(SwipeRefreshLayout.this.mCurrentTargetOffsetTop + SwipeRefreshLayout.this.getPaddingTop(), SwipeRefreshLayout.this.mReturnToStartPositionListener);
            }
        };
        this.mCancel = new Runnable() {
            public void run() {
                SwipeRefreshLayout.this.mReturningToStart = true;
                if (SwipeRefreshLayout.this.mProgressBar != null) {
                    SwipeRefreshLayout.this.mFromPercentage = SwipeRefreshLayout.this.mCurrPercentage;
                    SwipeRefreshLayout.this.mShrinkTrigger.setDuration((long) SwipeRefreshLayout.this.mMediumAnimationDuration);
                    SwipeRefreshLayout.this.mShrinkTrigger.setAnimationListener(SwipeRefreshLayout.this.mShrinkAnimationListener);
                    SwipeRefreshLayout.this.mShrinkTrigger.reset();
                    SwipeRefreshLayout.this.mShrinkTrigger.setInterpolator(SwipeRefreshLayout.this.mDecelerateInterpolator);
                    SwipeRefreshLayout.this.startAnimation(SwipeRefreshLayout.this.mShrinkTrigger);
                }
                SwipeRefreshLayout.this.animateOffsetToStartPosition(SwipeRefreshLayout.this.mCurrentTargetOffsetTop + SwipeRefreshLayout.this.getPaddingTop(), SwipeRefreshLayout.this.mReturnToStartPositionListener);
            }
        };
        this.mTouchSlop = ViewConfiguration.get(context).getScaledTouchSlop();
        this.mMediumAnimationDuration = getResources().getInteger(17694721);
        setWillNotDraw(false);
        this.mProgressBar = new SwipeProgressBar(this);
        this.mProgressBarHeight = (int) (getResources().getDisplayMetrics().density * PROGRESS_BAR_HEIGHT);
        this.mDecelerateInterpolator = new DecelerateInterpolator(DECELERATE_INTERPOLATION_FACTOR);
        this.mAccelerateInterpolator = new AccelerateInterpolator(ACCELERATE_INTERPOLATION_FACTOR);
        TypedArray a = context.obtainStyledAttributes(attrs, LAYOUT_ATTRS);
        setEnabled(a.getBoolean(0, true));
        a.recycle();
    }

    public void onAttachedToWindow() {
        super.onAttachedToWindow();
        removeCallbacks(this.mCancel);
        removeCallbacks(this.mReturnToStartPosition);
    }

    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        removeCallbacks(this.mReturnToStartPosition);
        removeCallbacks(this.mCancel);
    }

    private void animateOffsetToStartPosition(int from, AnimationListener listener) {
        this.mFrom = from;
        this.mAnimateToStartPosition.reset();
        this.mAnimateToStartPosition.setDuration((long) this.mMediumAnimationDuration);
        this.mAnimateToStartPosition.setAnimationListener(listener);
        this.mAnimateToStartPosition.setInterpolator(this.mDecelerateInterpolator);
        this.mTarget.startAnimation(this.mAnimateToStartPosition);
    }

    public void setOnRefreshListener(OnRefreshListener listener) {
        this.mListener = listener;
    }

    private void setTriggerPercentage(float percent) {
        if (percent == 0.0f) {
            this.mCurrPercentage = 0.0f;
            return;
        }
        this.mCurrPercentage = percent;
        this.mProgressBar.setTriggerPercentage(percent);
    }

    public void setRefreshing(boolean refreshing) {
        if (this.mRefreshing != refreshing) {
            ensureTarget();
            this.mCurrPercentage = 0.0f;
            this.mRefreshing = refreshing;
            if (this.mRefreshing) {
                this.mProgressBar.start();
            } else {
                this.mProgressBar.stop();
            }
        }
    }

    public void setColorScheme(int colorRes1, int colorRes2, int colorRes3, int colorRes4) {
        ensureTarget();
        Resources res = getResources();
        this.mProgressBar.setColorScheme(res.getColor(colorRes1), res.getColor(colorRes2), res.getColor(colorRes3), res.getColor(colorRes4));
    }

    public boolean isRefreshing() {
        return this.mRefreshing;
    }

    private void ensureTarget() {
        if (this.mTarget == null) {
            if (getChildCount() <= 1 || isInEditMode()) {
                this.mTarget = getChildAt(0);
                this.mOriginalOffsetTop = this.mTarget.getTop() + getPaddingTop();
            } else {
                throw new IllegalStateException("SwipeRefreshLayout can host only one direct child");
            }
        }
        if (this.mDistanceToTriggerSync == GroundOverlayOptions.NO_DIMENSION && getParent() != null && ((View) getParent()).getHeight() > 0) {
            this.mDistanceToTriggerSync = (float) ((int) Math.min(((float) ((View) getParent()).getHeight()) * MAX_SWIPE_DISTANCE_FACTOR, BitmapDescriptorFactory.HUE_GREEN * getResources().getDisplayMetrics().density));
        }
    }

    public void draw(Canvas canvas) {
        super.draw(canvas);
        this.mProgressBar.draw(canvas);
    }

    protected void onLayout(boolean changed, int left, int top, int right, int bottom) {
        int width = getMeasuredWidth();
        int height = getMeasuredHeight();
        this.mProgressBar.setBounds(0, 0, width, this.mProgressBarHeight);
        if (getChildCount() != 0) {
            View child = getChildAt(0);
            int childLeft = getPaddingLeft();
            int childTop = this.mCurrentTargetOffsetTop + getPaddingTop();
            child.layout(childLeft, childTop, childLeft + ((width - getPaddingLeft()) - getPaddingRight()), childTop + ((height - getPaddingTop()) - getPaddingBottom()));
        }
    }

    public void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
        if (getChildCount() > 1 && !isInEditMode()) {
            throw new IllegalStateException("SwipeRefreshLayout can host only one direct child");
        } else if (getChildCount() > 0) {
            getChildAt(0).measure(MeasureSpec.makeMeasureSpec((getMeasuredWidth() - getPaddingLeft()) - getPaddingRight(), 1073741824), MeasureSpec.makeMeasureSpec((getMeasuredHeight() - getPaddingTop()) - getPaddingBottom(), 1073741824));
        }
    }

    public boolean canChildScrollUp() {
        if (VERSION.SDK_INT >= 14) {
            return ViewCompat.canScrollVertically(this.mTarget, -1);
        }
        if (this.mTarget instanceof AbsListView) {
            AbsListView absListView = this.mTarget;
            if (absListView.getChildCount() <= 0 || (absListView.getFirstVisiblePosition() <= 0 && absListView.getChildAt(0).getTop() >= absListView.getPaddingTop())) {
                return false;
            }
            return true;
        } else if (this.mTarget.getScrollY() <= 0) {
            return false;
        } else {
            return true;
        }
    }

    public boolean onInterceptTouchEvent(MotionEvent ev) {
        ensureTarget();
        boolean handled = false;
        if (this.mReturningToStart && ev.getAction() == 0) {
            this.mReturningToStart = false;
        }
        if (!(!isEnabled() || this.mReturningToStart || canChildScrollUp())) {
            handled = onTouchEvent(ev);
        }
        return !handled ? super.onInterceptTouchEvent(ev) : handled;
    }

    public void requestDisallowInterceptTouchEvent(boolean b) {
    }

    public boolean onTouchEvent(MotionEvent event) {
        switch (event.getAction()) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                this.mCurrPercentage = 0.0f;
                this.mDownEvent = MotionEvent.obtain(event);
                this.mPrevY = this.mDownEvent.getY();
                return false;
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
            case DetectedActivity.STILL /*3*/:
                if (this.mDownEvent == null) {
                    return false;
                }
                this.mDownEvent.recycle();
                this.mDownEvent = null;
                return false;
            case DetectedActivity.ON_FOOT /*2*/:
                if (this.mDownEvent == null || this.mReturningToStart) {
                    return false;
                }
                float eventY = event.getY();
                float yDiff = eventY - this.mDownEvent.getY();
                if (yDiff <= ((float) this.mTouchSlop)) {
                    return false;
                }
                if (yDiff > this.mDistanceToTriggerSync) {
                    startRefresh();
                    return true;
                }
                setTriggerPercentage(this.mAccelerateInterpolator.getInterpolation(yDiff / this.mDistanceToTriggerSync));
                float offsetTop = yDiff;
                if (this.mPrevY > eventY) {
                    offsetTop = yDiff - ((float) this.mTouchSlop);
                }
                updateContentOffsetTop((int) offsetTop);
                if (this.mPrevY <= eventY || this.mTarget.getTop() >= this.mTouchSlop) {
                    updatePositionTimeout();
                } else {
                    removeCallbacks(this.mCancel);
                }
                this.mPrevY = event.getY();
                return true;
            default:
                return false;
        }
    }

    private void startRefresh() {
        removeCallbacks(this.mCancel);
        this.mReturnToStartPosition.run();
        setRefreshing(true);
        this.mListener.onRefresh();
    }

    private void updateContentOffsetTop(int targetTop) {
        int currentTop = this.mTarget.getTop();
        if (((float) targetTop) > this.mDistanceToTriggerSync) {
            targetTop = (int) this.mDistanceToTriggerSync;
        } else if (targetTop < 0) {
            targetTop = 0;
        }
        setTargetOffsetTopAndBottom(targetTop - currentTop);
    }

    private void setTargetOffsetTopAndBottom(int offset) {
        this.mTarget.offsetTopAndBottom(offset);
        this.mCurrentTargetOffsetTop = this.mTarget.getTop();
    }

    private void updatePositionTimeout() {
        removeCallbacks(this.mCancel);
        postDelayed(this.mCancel, RETURN_TO_ORIGINAL_POSITION_TIMEOUT);
    }
}
