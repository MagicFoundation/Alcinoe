package com.alcinoe.widget;

import android.widget.EditText;
import android.util.AttributeSet;
import android.content.Context;
import android.os.ResultReceiver;
import android.os.Bundle;
import android.view.inputmethod.InputMethodManager;
import android.view.KeyEvent;
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

}
