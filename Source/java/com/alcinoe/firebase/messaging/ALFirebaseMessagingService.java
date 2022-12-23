package com.alcinoe.firebase.messaging;

import java.util.Map;
import org.json.JSONObject;
import org.json.JSONArray;
import androidx.lifecycle.MutableLiveData;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import com.google.firebase.messaging.FirebaseMessagingService;
import com.google.firebase.messaging.RemoteMessage;

public class ALFirebaseMessagingService extends FirebaseMessagingService {
        
  public static final MutableLiveData<String> newTokenDispatcher = new MutableLiveData<>();
  public static final MutableLiveData<String> newMessageDispatcher = new MutableLiveData<>();
    
  private static final String TAG = "ALFirebaseMessagingService";
      
  /**
   * Called when message is received.
   * @param remoteMessage Object representing the message received from Firebase Cloud Messaging.
   */
  @Override
  public void onMessageReceived(RemoteMessage remoteMessage){

    // There are two types of messages data messages and notification messages. Data messages are handled
    // here in onMessageReceived whether the app is in the foreground or background. Data messages are the type
    // traditionally used with GCM. Notification messages are only received here in onMessageReceived when the app
    // is in the foreground. When the app is in the background an automatically generated notification is displayed.
    // When the user taps on the notification they are returned to the app. Messages containing both notification
    // and data payloads are treated as notification messages. The Firebase console always sends notification
    // messages. For more see: https://firebase.google.com/docs/cloud-messaging/concept-options
        
    try{
    
      //init MapData
      Map<String, String> MapData = remoteMessage.getData();
    
      //init jsonData
      JSONObject jsonData = new JSONObject();
      for (Map.Entry<String, String> entry : MapData.entrySet()) {
        jsonData.put(entry.getKey(), entry.getValue()); /* String */
      }    
      jsonData.put("google.message_id", remoteMessage.getMessageId()); /* String */
      jsonData.put("google.sent_time", remoteMessage.getSentTime()); /* long */
      jsonData.put("google.ttl", remoteMessage.getTtl()); /* int */
             
      //dispatch the message in the main thread
      new Handler(Looper.getMainLooper()).post(new Runnable() {
        @Override
        public void run() {

          try{
            Log.v(TAG, "onMessageReceived"); 
            newMessageDispatcher.setValue(jsonData.toString()); 
          } catch (Throwable e){ 
            Log.e(TAG, "onMessageReceived - Exception", e); 
          }  

        }
      });

    } 
    catch (Throwable e){ Log.e(TAG, "onMessageReceived - Exception", e); }  

  }
  
  /**
   * Called when a new token for the default Firebase project is generated.
   * This is invoked after app install when a token is first generated, and again if the token changes.
   * @param The token used for sending messages to this application instance. This token is the same as the one retrieved by FirebaseMessaging#getToken().
   */
  @Override
  public void onNewToken(String token) {
    /* this event is called from a background thread */
    newTokenDispatcher.postValue(String.valueOf(token));
  }

}
