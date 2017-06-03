/*
 * Copyright (C) 2015 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.alcinoe.view;

import android.content.Context;
import android.graphics.Rect;
import android.view.ActionMode;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewConfiguration;
import android.os.Build;

import com.alcinoe.R;
import com.alcinoe.util.Preconditions;
import com.alcinoe.view.menu.MenuBuilder;
import com.alcinoe.widget.ALFloatingToolbar;

import java.util.Arrays;

public class ALFloatingActionMode extends ActionMode {

    private static final int MAX_HIDE_DURATION = 3000;
    private static final int MOVING_HIDE_DELAY = 300;

    private final Context mContext;
    private final ALActionMode.Callback2 mCallback;
    private final MenuBuilder mMenu;
    private final Rect mContentRect;
    private final Rect mContentRectOnScreen;
    private final Rect mPreviousContentRectOnScreen;
    private final int[] mViewPositionOnScreen;
    private final int[] mPreviousViewPositionOnScreen;
    private final int[] mRootViewPositionOnScreen;
    private final Rect mViewRectOnScreen;
    private final Rect mPreviousViewRectOnScreen;
    private final Rect mScreenRect;
    private final View mOriginatingView;
    private final int mBottomAllowance;

    private final Runnable mMovingOff = new Runnable() {
        public void run() {
            mFloatingToolbarVisibilityHelper.setMoving(false);
            mFloatingToolbarVisibilityHelper.updateToolbarVisibility();
        }
    };

    private final Runnable mHideOff = new Runnable() {
        public void run() {
            mFloatingToolbarVisibilityHelper.setHideRequested(false);
            mFloatingToolbarVisibilityHelper.updateToolbarVisibility();
        }
    };

    private ALFloatingToolbar mFloatingToolbar;
    private FloatingToolbarVisibilityHelper mFloatingToolbarVisibilityHelper;

    public ALFloatingActionMode(
            Context context, ALActionMode.Callback2 callback, View originatingView) {
        mContext = Preconditions.checkNotNull(context);
        mCallback = Preconditions.checkNotNull(callback);
        mMenu = new MenuBuilder(context).setDefaultShowAsAction(
                MenuItem.SHOW_AS_ACTION_IF_ROOM);
        if (Build.VERSION.SDK_INT >= 23) {
          setType(ActionMode.TYPE_FLOATING);
        }
        mContentRect = new Rect();
        mContentRectOnScreen = new Rect();
        mPreviousContentRectOnScreen = new Rect();
        mViewPositionOnScreen = new int[2];
        mPreviousViewPositionOnScreen = new int[2];
        mRootViewPositionOnScreen = new int[2];
        mViewRectOnScreen = new Rect();
        mPreviousViewRectOnScreen = new Rect();
        mScreenRect = new Rect();
        mOriginatingView = Preconditions.checkNotNull(originatingView);
        mOriginatingView.getLocationOnScreen(mViewPositionOnScreen);
        // Allow the content rect to overshoot a little bit beyond the
        // bottom view bound if necessary.
        mBottomAllowance = context.getResources()
                .getDimensionPixelSize(R.dimen.al_content_rect_bottom_clip_allowance);
    }

    public void setFloatingToolbar(ALFloatingToolbar floatingToolbar) {
        mFloatingToolbar = floatingToolbar
                .setMenu(mMenu)
                .setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
                        @Override
                    public boolean onMenuItemClick(MenuItem item) {
                        return mCallback.onActionItemClicked(ALFloatingActionMode.this, item);
                    }
                });
        mFloatingToolbarVisibilityHelper = new FloatingToolbarVisibilityHelper(mFloatingToolbar);
        mFloatingToolbarVisibilityHelper.activate();
    }

    @Override
    public void setTitle(CharSequence title) {}

    @Override
    public void setTitle(int resId) {}

    @Override
    public void setSubtitle(CharSequence subtitle) {}

    @Override
    public void setSubtitle(int resId) {}

    @Override
    public void setCustomView(View view) {}

    @Override
    public void invalidate() {
        checkToolbarInitialized();
        mCallback.onPrepareActionMode(this, mMenu);
        invalidateContentRect();  // Will re-layout and show the toolbar if necessary.
    }

    @Override
    public void invalidateContentRect() {
        checkToolbarInitialized();
        mCallback.onGetContentRect(this, mOriginatingView, mContentRect);
        repositionToolbar();
    }

    public void updateViewLocationInWindow() {
        checkToolbarInitialized();

        mOriginatingView.getLocationOnScreen(mViewPositionOnScreen);
        mOriginatingView.getRootView().getLocationOnScreen(mRootViewPositionOnScreen);
        mOriginatingView.getGlobalVisibleRect(mViewRectOnScreen);
        mViewRectOnScreen.offset(mRootViewPositionOnScreen[0], mRootViewPositionOnScreen[1]);

        if (!Arrays.equals(mViewPositionOnScreen, mPreviousViewPositionOnScreen)
                || !mViewRectOnScreen.equals(mPreviousViewRectOnScreen)) {
            repositionToolbar();
            mPreviousViewPositionOnScreen[0] = mViewPositionOnScreen[0];
            mPreviousViewPositionOnScreen[1] = mViewPositionOnScreen[1];
            mPreviousViewRectOnScreen.set(mViewRectOnScreen);
        }
    }

    private void repositionToolbar() {
        checkToolbarInitialized();

        mContentRectOnScreen.set(mContentRect);
        mContentRectOnScreen.offset(mViewPositionOnScreen[0], mViewPositionOnScreen[1]);

        if (isContentRectWithinBounds()) {
            mFloatingToolbarVisibilityHelper.setOutOfBounds(false);
            // Make sure that content rect is not out of the view's visible bounds.
            mContentRectOnScreen.set(
                    Math.max(mContentRectOnScreen.left, mViewRectOnScreen.left),
                    Math.max(mContentRectOnScreen.top, mViewRectOnScreen.top),
                    Math.min(mContentRectOnScreen.right, mViewRectOnScreen.right),
                    Math.min(mContentRectOnScreen.bottom,
                            mViewRectOnScreen.bottom + mBottomAllowance));

            if (!mContentRectOnScreen.equals(mPreviousContentRectOnScreen)) {
                // Content rect is moving.
                mOriginatingView.removeCallbacks(mMovingOff);
                mFloatingToolbarVisibilityHelper.setMoving(true);
                mFloatingToolbarVisibilityHelper.updateToolbarVisibility();
                mOriginatingView.postDelayed(mMovingOff, MOVING_HIDE_DELAY);

                mFloatingToolbar.setContentRect(mContentRectOnScreen);
                mFloatingToolbar.updateLayout();
            }
        } else {
            mFloatingToolbarVisibilityHelper.setOutOfBounds(true);
            mFloatingToolbarVisibilityHelper.updateToolbarVisibility();
            mContentRectOnScreen.setEmpty();
        }

        mPreviousContentRectOnScreen.set(mContentRectOnScreen);
    }

    private boolean isContentRectWithinBounds() {
        mScreenRect.set(
            0,
            0,
            mContext.getResources().getDisplayMetrics().widthPixels,
            mContext.getResources().getDisplayMetrics().heightPixels);

        return intersectsClosed(mContentRectOnScreen, mScreenRect)
            && intersectsClosed(mContentRectOnScreen, mViewRectOnScreen);
    }

    /*
     * Same as Rect.intersects, but includes cases where the rectangles touch.
    */
    private static boolean intersectsClosed(Rect a, Rect b) {
         return a.left <= b.right && b.left <= a.right
                 && a.top <= b.bottom && b.top <= a.bottom;
    }

    @Override
    public void hide(long duration) {
        checkToolbarInitialized();

        if (duration == ActionMode.DEFAULT_HIDE_DURATION) {
            duration = ViewConfiguration.getDefaultActionModeHideDuration();
        }
        duration = Math.min(MAX_HIDE_DURATION, duration);
        mOriginatingView.removeCallbacks(mHideOff);
        if (duration <= 0) {
            mHideOff.run();
        } else {
            mFloatingToolbarVisibilityHelper.setHideRequested(true);
            mFloatingToolbarVisibilityHelper.updateToolbarVisibility();
            mOriginatingView.postDelayed(mHideOff, duration);
        }
    }

    @Override
    public void onWindowFocusChanged(boolean hasWindowFocus) {
        checkToolbarInitialized();
        mFloatingToolbarVisibilityHelper.setWindowFocused(hasWindowFocus);
        mFloatingToolbarVisibilityHelper.updateToolbarVisibility();
    }

    @Override
    public void finish() {
        checkToolbarInitialized();
        reset();
        mCallback.onDestroyActionMode(this);
    }

    @Override
    public Menu getMenu() {
        return mMenu;
    }

    @Override
    public CharSequence getTitle() {
        return null;
    }

    @Override
    public CharSequence getSubtitle() {
        return null;
    }

    @Override
    public View getCustomView() {
        return null;
    }

    @Override
    public MenuInflater getMenuInflater() {
        return new MenuInflater(mContext);
    }

    /**
     * @throws IllegalStateException
     */
    private void checkToolbarInitialized() {
        Preconditions.checkState(mFloatingToolbar != null);
        Preconditions.checkState(mFloatingToolbarVisibilityHelper != null);
    }

    private void reset() {
        mFloatingToolbar.dismiss();
        mFloatingToolbarVisibilityHelper.deactivate();
        mOriginatingView.removeCallbacks(mMovingOff);
        mOriginatingView.removeCallbacks(mHideOff);
    }

    /**
     * A helper for showing/hiding the floating toolbar depending on certain states.
     */
    private static final class FloatingToolbarVisibilityHelper {

        private final ALFloatingToolbar mToolbar;

        private boolean mHideRequested;
        private boolean mMoving;
        private boolean mOutOfBounds;
        private boolean mWindowFocused = true;

        private boolean mActive;

        public FloatingToolbarVisibilityHelper(ALFloatingToolbar toolbar) {
            mToolbar = Preconditions.checkNotNull(toolbar);
        }

        public void activate() {
            mHideRequested = false;
            mMoving = false;
            mOutOfBounds = false;
            mWindowFocused = true;

            mActive = true;
        }

        public void deactivate() {
            mActive = false;
            mToolbar.dismiss();
        }

        public void setHideRequested(boolean hide) {
            mHideRequested = hide;
        }

        public void setMoving(boolean moving) {
            mMoving = moving;
        }

        public void setOutOfBounds(boolean outOfBounds) {
            mOutOfBounds = outOfBounds;
        }

        public void setWindowFocused(boolean windowFocused) {
            mWindowFocused = windowFocused;
        }

        public void updateToolbarVisibility() {
            if (!mActive) {
                return;
            }

            if (mHideRequested || mMoving || mOutOfBounds || !mWindowFocused) {
                mToolbar.hide();
            } else {
                mToolbar.show();
            }
        }
    }
}
