package com.alcinoe.widget;

import android.widget.EditText;
import android.util.AttributeSet;
import android.content.Context;
import android.os.ResultReceiver;
import android.os.Bundle;
import android.view.inputmethod.InputMethodManager;
import com.alcinoe.view.inputmethod.ALSoftInputListener;

public class ALEditText extends EditText {
  private ALSoftInputListener mSoftInputListener;
    
  private static class SoftInputReceiver extends ResultReceiver {
    private static final int RESULT_UNCHANGED_HIDDEN = 1;
    private static final int RESULT_SHOWN = 2;
    private static final int RESULT_HIDDEN = 3;
    private static final int RESULT_UNCHANGED_SHOWN = 0;
    private ALSoftInputListener mListener;
   
    public SoftInputReceiver(ALSoftInputListener listener) {
      super(null);
      this.mListener = listener;
    }

    public void onReceiveResult(int result, Bundle data) {
      switch (result) {
        case RESULT_UNCHANGED_SHOWN /*0*/:
        case RESULT_SHOWN /*2*/:
          if (this.mListener != null) {
            this.mListener.onSoftInputShown();
          }
        case RESULT_UNCHANGED_HIDDEN /*1*/:
        case RESULT_HIDDEN /*3*/:
          if (this.mListener != null) {
            this.mListener.onSoftInputHidden();
          }
        default:
      }
    }
  }

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
