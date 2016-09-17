package com.alcinoe.widget;

import android.widget.EditText;
import android.util.AttributeSet;
import android.content.Context;
import android.os.ResultReceiver;
import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup;
import android.view.Menu;
import android.view.MenuItem;
import android.view.ActionMode;
import android.view.ActionMode.Callback;
import android.view.ActionMode.Callback2;
import android.view.inputmethod.InputMethodManager;
import android.view.KeyEvent;
import android.view.ViewParent;
import android.app.Activity;
import android.graphics.Rect;
import android.util.Log;
import com.alcinoe.view.inputmethod.ALSoftInputListener;
import com.alcinoe.text.method.ALKeyPreImeListener;

public class ALEditText extends EditText {
  private ALSoftInputListener mSoftInputListener;
  private ALKeyPreImeListener mKeyPreImeListener;
      
  public ALEditText(Context context){
    super(context);
  } 

  public ALEditText(Context context, AttributeSet attrs){
    super(context, attrs);
  } 
           
  public ALEditText(Context context, AttributeSet attrs, int defStyleAttr){
    super(context, attrs, defStyleAttr);
  } 
           
  public ALEditText(Context context, AttributeSet attrs, int defStyleAttr,  int defStyleRes){
    super(context, attrs, defStyleAttr, defStyleRes);
  } 

  @Override
  public boolean onKeyPreIme(int keyCode, KeyEvent event) {
    if (mKeyPreImeListener != null) return mKeyPreImeListener.onKeyPreIme(keyCode, event); 
    return super.onKeyPreIme(keyCode, event);
  }
  
  public void SetKeyPreImeListener(ALKeyPreImeListener listener) {
    this.mKeyPreImeListener = listener;
  }

  private static class SoftInputReceiver extends ResultReceiver {
    private ALSoftInputListener mListener;
   
    public SoftInputReceiver(ALSoftInputListener listener) {
      super(null);
      this.mListener = listener;
    }

    public void onReceiveResult(int result, Bundle data) {
      switch (result) {
        case InputMethodManager.RESULT_UNCHANGED_SHOWN:
        case InputMethodManager.RESULT_SHOWN: 
          if (this.mListener != null) this.mListener.onSoftInputShown(); 
          break;
        case InputMethodManager.RESULT_UNCHANGED_HIDDEN:
        case InputMethodManager.RESULT_HIDDEN:
          if (this.mListener != null) this.mListener.onSoftInputHidden();
          break;
      }
    }
  }

  public void showSoftInput() {
    SoftInputReceiver receiver = new SoftInputReceiver(this.mSoftInputListener);
    InputMethodManager imm = getInputMethodManager();
    imm.showSoftInput(this, 0, receiver);
  }

  public void HideSoftInput() {
    SoftInputReceiver receiver = new SoftInputReceiver(this.mSoftInputListener);
    InputMethodManager imm = getInputMethodManager();
    imm.hideSoftInputFromWindow(getWindowToken(), 0, receiver);
  }
  
  public void SetSoftInputListener(ALSoftInputListener listener) {
    this.mSoftInputListener = listener;
  }
  
  protected InputMethodManager getInputMethodManager() {
    return (InputMethodManager) getContext().getSystemService("input_method");
  }

  //when the view are added via windowManager.addView(view, Layout_Params)
  //then it's seam that startActionMode will not work by himself and we
  //must do the code below to start it (and you can not imagine how much hard i
  //worked to find this strange behavior). HOWEVER the other problem i have
  //if that on lollipop the copy/past popup is added in this ugly and not understandable
  //#{@#^ ACTIONBAR. the problem with this actionbar is that it's need to be in 
  //the decorview, but the actual framework of delphi don't permit this (if we
  //do so, it's will be simply drawed blank). If version < lollipop (22) then it's
  //no problem because it's will be normal popup and in version > lollipop (22) then
  //no problem also because it's will be floating actionbar like popup menu that
  //work ok with the delphi framework.
  //http://stackoverflow.com/questions/39506977/is-it-possible-to-show-the-android-actionbar-in-delphi-firemonkey-app
  //http://stackoverflow.com/questions/39517011/how-to-move-my-actionbar-in-my-own-dedicated-view
  //http://stackoverflow.com/questions/39396662/edittext-how-to-activate-copy-paste-popup-without-any-actionbar
  //so actually i decide to show the actionbar only via the floating way (this way)
  //all previous device right now no copy option (but past work). if someone want to implement
  //a copy/past menu for lollipop he is welcome 
  //http://stackoverflow.com/questions/39501339/how-to-replace-the-actionbar-by-a-popup-menu
  @Override
  public ActionMode startActionMode(ActionMode.Callback callback, int type) {    
    this.invalidate(); // << don't ask me why but without this the floating actionbar is not showed after the first
                       //    selection is maded (ie on the first double click on some word for exemple) in the EditText
    ViewParent parent = getParent();
    if (parent == null) return null;
    Activity activity = (Activity) this.getContext(); 
    ViewGroup decoreview = (ViewGroup) activity.getWindow().getDecorView();
    try {
        return decoreview.startActionModeForChild(this, callback, type); 
    } catch (AbstractMethodError ame) {
        // Older implementations of custom views might not implement this.
        return decoreview.startActionModeForChild(this, callback); 
    }
  }
}
