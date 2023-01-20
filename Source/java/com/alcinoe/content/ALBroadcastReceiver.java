package com.alcinoe.content;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

public class ALBroadcastReceiver extends BroadcastReceiver {
	
  ALBroadcastReceiverListener mListener;
	
  public ALBroadcastReceiver() {
    super(); 
    this.mListener = null;
  }    
  
  public void setListener(ALBroadcastReceiverListener listener) {
    mListener = listener;
  }    
  
  @Override
  public void onReceive(Context context, Intent intent) {
    if (mListener != null) { mListener.onReceive(context, intent); }
  }

}
