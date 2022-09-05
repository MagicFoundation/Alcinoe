package com.alcinoe.edittext;

import android.widget.EditText;
import android.util.AttributeSet;
import android.content.Context;
import android.view.KeyEvent;
import android.text.InputFilter;


public class ALEditText extends EditText {
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
    
}
