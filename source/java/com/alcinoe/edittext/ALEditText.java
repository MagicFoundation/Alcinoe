package com.alcinoe.edittext;

import android.widget.EditText;
import android.util.AttributeSet;
import android.content.Context;
import android.view.ViewGroup;
import android.view.ActionMode;
import android.view.inputmethod.InputMethodManager;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewParent;
import android.app.Activity;
import android.graphics.Rect;
import android.util.DisplayMetrics;
import android.util.Log;
import android.os.Build;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputConnection;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;
import android.view.ViewTreeObserver.OnPreDrawListener;
import android.text.InputFilter;
import com.alcinoe.view.inputmethod.ALSoftInputListener;
import com.alcinoe.view.ALFloatingActionMode;
import com.alcinoe.view.ALActionMode;
import com.alcinoe.widget.ALFloatingToolbar;


public class ALEditText extends EditText {
  private ALSoftInputListener mSoftInputListener;
  private ALKeyPreImeListener mKeyPreImeListener;
  private OnGlobalLayoutListener mOnGlobalLayoutListener;
  private ALFloatingActionMode mFloatingActionMode;
  private View mFloatingActionModeOriginatingView;
  private OnPreDrawListener mFloatingToolbarPreDrawListener;
  private ALFloatingToolbar mFloatingToolbar;    
  private Context mContext;    
  private int mDefStyleAttr;

  public ALEditText(Context context){
    super(context);
    mContext = context;
    mDefStyleAttr = 0;
    mOnGlobalLayoutListener = null;  
  } 

  public ALEditText(Context context, AttributeSet attrs){
    super(context, attrs);
    mContext = context;
    mDefStyleAttr = 0;
    mOnGlobalLayoutListener = null;  
  } 
           
  public ALEditText(Context context, AttributeSet attrs, int defStyleAttr){
    super(context, attrs, defStyleAttr);
    mContext = context;
    mDefStyleAttr = defStyleAttr;
    mOnGlobalLayoutListener = null;  
  } 
           
  public ALEditText(Context context, AttributeSet attrs, int defStyleAttr,  int defStyleRes){
    super(context, attrs, defStyleAttr, defStyleRes);
    mContext = context;
    mDefStyleAttr = defStyleAttr;
    mOnGlobalLayoutListener = null;  
  } 
  
  public void setMaxLength(int value) {
  	if (value > 0) {
  		InputFilter[] filterArray = new InputFilter[1];
  		filterArray[0] = new InputFilter.LengthFilter(value);
  		setFilters(filterArray);
  	}
  	else {
  		setFilters(new InputFilter[] {});
  	}
  }

  @Override
  public boolean onKeyPreIme(int keyCode, KeyEvent event) {
    if (mKeyPreImeListener != null) return mKeyPreImeListener.onKeyPreIme(keyCode, event); 
    return super.onKeyPreIme(keyCode, event);
  }
  
  public void setKeyPreImeListener(ALKeyPreImeListener listener) {
    this.mKeyPreImeListener = listener;
  }

  public void showSoftInput() {
    InputMethodManager imm = getInputMethodManager();
    imm.showSoftInput(this, InputMethodManager.SHOW_FORCED);
  }

  public void hideSoftInput() {
    InputMethodManager imm = getInputMethodManager();
    imm.hideSoftInputFromWindow(getWindowToken(), 0);
  }
  
  public void setSoftInputListener(ALSoftInputListener listener) {
    this.mSoftInputListener = listener;
    
    final Activity activity = (Activity) this.getContext(); 
    final ViewGroup decoreview = (ViewGroup) activity.getWindow().getDecorView();

    if (this.mOnGlobalLayoutListener != null) {
      decoreview.getViewTreeObserver().removeOnGlobalLayoutListener(this.mOnGlobalLayoutListener);
      this.mOnGlobalLayoutListener = null;    
    }

    if (listener != null) {
      this.mOnGlobalLayoutListener = new OnGlobalLayoutListener() {

          private float convertPixelsToDp(float px, Context context){
            DisplayMetrics metrics = context.getResources().getDisplayMetrics();
            float dp = px / ((float)metrics.densityDpi / DisplayMetrics.DENSITY_DEFAULT);
            return dp;
          }

          @Override
          public void onGlobalLayout() {              
              Rect contentrect = new Rect();
              Rect totalrect = new Rect();
              decoreview.getWindowVisibleDisplayFrame(contentrect);
              decoreview.getDrawingRect(totalrect);
              if (convertPixelsToDp(totalrect.height(), (Context) activity) - convertPixelsToDp(contentrect.height(), (Context) activity) > 120) { mSoftInputListener.onSoftInputShown(); }
              else {mSoftInputListener.onSoftInputHidden(); }
           }
      };
      decoreview.getViewTreeObserver().addOnGlobalLayoutListener(this.mOnGlobalLayoutListener);  
    }

  }
  
  protected InputMethodManager getInputMethodManager() {
    return (InputMethodManager) getContext().getSystemService(mContext.INPUT_METHOD_SERVICE);
  }

  @Override
  public InputConnection onCreateInputConnection(EditorInfo outAttrs) {
    InputConnection connection = super.onCreateInputConnection(outAttrs);
    int imeActions = outAttrs.imeOptions&EditorInfo.IME_MASK_ACTION;
    if ((imeActions&EditorInfo.IME_ACTION_DONE) != 0) {
      outAttrs.imeOptions ^= imeActions;
      outAttrs.imeOptions |= EditorInfo.IME_ACTION_DONE;
      if ((outAttrs.imeOptions&EditorInfo.IME_FLAG_NO_ENTER_ACTION) != 0) {
          outAttrs.imeOptions &= ~EditorInfo.IME_FLAG_NO_ENTER_ACTION;
      }
    }
    return connection;
  }

  // when the view are added via windowManager.addView(view, Layout_Params)
  // then it's seam that startActionMode will not work by himself and we
  // must do the code below to start it (and you can not imagine how much hard i
  // worked to find this strange behavior). HOWEVER the other problem i have
  // if that on lollipop the copy/past popup is added in this ugly and not understandable
  // #{@#^ ACTIONBAR. the problem with this actionbar is that it's need to be in 
  // the decorview, but the actual framework of delphi don't permit this (if we
  // do so, it's will be simply drawed blank). If version < lollipop (22) then it's
  // no problem because it's will be normal popup and in version > lollipop (22) then
  // no problem also because it's will be floating actionbar like popup menu that
  // work ok with the delphi framework.
  // http://stackoverflow.com/questions/39396662/edittext-how-to-activate-copy-paste-popup-without-any-actionbar
  // so actually i decide to show the actionbar only via the floating way
  // to backport the floating actionbar of android api > 23 to lollipop (api 22) and lowest device
  // i copy the full source code of marshmallow floating action bar in alcinoe. I was also forced
  // to update the original source code because the floating action node was made like the sub Window
  // of the decorview, and in this way it's draw BEHIND our EditText
  // https://stackoverflow.com/questions/39561133/why-my-edittext-copy-paste-menu-is-under-the-edittext-how-change-the-z-order-of
  // i replace in floatingtoolbar the type of the windows from TYPE_APPLICATION_ABOVE_SUB_PANEL to 
  // TYPE_APPLICATION
  @Override
  public ActionMode startActionMode(ActionMode.Callback callback) {    

    ALActionMode.Callback2 wrappedCallback = new ActionModeCallback2Wrapper(callback);
    ALFloatingActionMode mode = null;
    mode = createFloatingActionMode(this, wrappedCallback);
    if (mode != null && wrappedCallback.onCreateActionMode(mode, mode.getMenu())) {
        setHandledFloatingActionMode(mode);
    } else {
        mode = null;
    }
    return mode;
  
  }

  @Override
  public ActionMode startActionMode(ActionMode.Callback callback, int type) {    
    return startActionMode(callback);    
  }

  private void cleanupFloatingActionModeViews() {
      if (mFloatingToolbar != null) {
          mFloatingToolbar.dismiss();
          mFloatingToolbar = null;
      }
      if (mFloatingActionModeOriginatingView != null) {
          if (mFloatingToolbarPreDrawListener != null) {
              mFloatingActionModeOriginatingView.getViewTreeObserver()
                  .removeOnPreDrawListener(mFloatingToolbarPreDrawListener);
              mFloatingToolbarPreDrawListener = null;
          }
          mFloatingActionModeOriginatingView = null;
      }
  }
  

  @Override
  public void onWindowFocusChanged(boolean hasWindowFocus) {
      super.onWindowFocusChanged(hasWindowFocus);
      if (mFloatingActionMode != null) {
          mFloatingActionMode.onWindowFocusChanged(hasWindowFocus);
      }
  }
  
  @Override
  protected void onDetachedFromWindow() {
    super.onDetachedFromWindow();

    if (mFloatingToolbar != null) {
        mFloatingToolbar.dismiss();
        mFloatingToolbar = null;
    }
  }

  private ALFloatingActionMode createFloatingActionMode(
          View originatingView, ALActionMode.Callback2 callback) {
      if (mFloatingActionMode != null) {
          mFloatingActionMode.finish();
      }
      cleanupFloatingActionModeViews();
      final ALFloatingActionMode mode =
              new ALFloatingActionMode(mContext, callback, originatingView);
      mFloatingActionModeOriginatingView = originatingView;
      mFloatingToolbarPreDrawListener =
          new OnPreDrawListener() {
              @Override
              public boolean onPreDraw() {
                  mode.updateViewLocationInWindow();
                  return true;
              }
          };
      return mode;
  } 

  private void setHandledFloatingActionMode(ALFloatingActionMode mode) {
      mFloatingActionMode = mode;
      mFloatingToolbar = new ALFloatingToolbar(mContext, ((Activity) mContext).getWindow(), mDefStyleAttr);
      mFloatingActionMode.setFloatingToolbar(mFloatingToolbar);
      mFloatingActionMode.invalidate();  // Will show the floating toolbar if necessary.
      mFloatingActionModeOriginatingView.getViewTreeObserver()
          .addOnPreDrawListener(mFloatingToolbarPreDrawListener);
  }

  /**
   * Clears out internal references when the action mode is destroyed.
   */
  private class ActionModeCallback2Wrapper extends ALActionMode.Callback2 {
      private final ActionMode.Callback mWrapped;

      public ActionModeCallback2Wrapper(ActionMode.Callback wrapped) {
          mWrapped = wrapped;
      }

      public boolean onCreateActionMode(ActionMode mode, Menu menu) {
          return mWrapped.onCreateActionMode(mode, menu);
      }

      @SuppressWarnings("deprecation")
      public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
          requestFitSystemWindows();
          return mWrapped.onPrepareActionMode(mode, menu);
      }

      public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
          return mWrapped.onActionItemClicked(mode, item);
      }

      @SuppressWarnings("deprecation")
      public void onDestroyActionMode(ActionMode mode) {
          mWrapped.onDestroyActionMode(mode);
          cleanupFloatingActionModeViews();
          mFloatingActionMode = null;
          requestFitSystemWindows();
      }

      @Override
      public void onGetContentRect(ActionMode mode, View view, Rect outRect) {
          if (Build.VERSION.SDK_INT >= 23) {
            if (mWrapped instanceof ActionMode.Callback2) {
                ((ActionMode.Callback2) mWrapped).onGetContentRect(mode, view, outRect);
            } else {
                super.onGetContentRect(mode, view, outRect);
            }
          }
          else {
            super.onGetContentRect(mode, view, outRect);            
          }
      }
  }

}
