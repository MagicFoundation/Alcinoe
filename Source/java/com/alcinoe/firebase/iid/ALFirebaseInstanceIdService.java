package com.alcinoe.firebase.iid;

import com.google.firebase.iid.FirebaseInstanceId;
import com.google.firebase.iid.FirebaseInstanceIdService;
import android.support.v4.content.LocalBroadcastManager;
import android.content.Intent;

public class ALFirebaseInstanceIdService extends FirebaseInstanceIdService {

  public static final String ACTION_TOKENREFRESHED = "com.alcinoe.firebase.iid.FirebaseInstanceId.tokenRefreshed";

  /**
   * Called if InstanceID token is updated. This may occur if the security of
   * the previous token had been compromised. Note that this is called when the InstanceID token
   * is initially generated so this is where you would retrieve the token.
   */
  @Override
  public void onTokenRefresh() {
      
    /* build the intent */
    Intent intent = new Intent(ACTION_TOKENREFRESHED);
    intent.putExtra("token", FirebaseInstanceId.getInstance().getToken()); /* String */
    
    /* send the data to registered receivers */
    try{
      LocalBroadcastManager.getInstance(this).sendBroadcast(intent);
    } catch (Throwable e){
      //no exception handling
    }  
  
  }
       
}