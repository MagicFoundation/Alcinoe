package com.alcinoe.firebase.messaging;

import java.util.Map;
import java.io.FileWriter;
import java.io.File;
import java.io.FileInputStream;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import org.json.JSONObject;
import org.json.JSONArray;
import androidx.lifecycle.MutableLiveData;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.os.Build;
import com.google.firebase.messaging.FirebaseMessagingService;
import com.google.firebase.messaging.RemoteMessage;

public class ALFirebaseMessagingService extends FirebaseMessagingService {
        
  public static final MutableLiveData<String> newTokenDispatcher = new MutableLiveData<>();
  public static final MutableLiveData<String> newMessageDispatcher = new MutableLiveData<>();
    
  private static final String TAG = "ALFirebaseMessagingService";
  private final static Lock lock = new ReentrantLock(); 
  private static String pendingDataMessagesFilename = "";
    
  /**
   * onCreate
   */  
  @Override
  public void onCreate() {
    super.onCreate();
    this.pendingDataMessagesFilename = "/data/data/" + getApplicationContext().getPackageName() + "/files/ALFirebaseMessagingService.json";  
  }
  
  /**
   * internal function to load string from a file. lock must be set before
   * calling this function
   */  
  private static String getPendingDataMessagesInternal() {

    String jsonTxt = "{messages:[]}";

    try{

      File jsonFile = new File(pendingDataMessagesFilename);
      if (jsonFile.exists()) {

        FileInputStream fis = new FileInputStream(jsonFile);
        int size = fis.available();
        byte[] buffer = new byte[size];
        fis.read(buffer);
        fis.close();
        jsonTxt = new String(buffer, "UTF-8");
   
      }

    } 
    catch (Throwable e){ Log.e(TAG, "getPendingDataMessagesInternal - Exception", e); }  
      
    return jsonTxt;  
    
  }
  
  /**
   * return an json string of all the pending data messages that was not 
   * yet delivered because the app was in background or not running
   * delete also the message from the queue.
   */  
  public static String getPendingDataMessages() {
    
    String jsonTxt = "";
    
    lock.lock();
    try {
      
      jsonTxt = getPendingDataMessagesInternal();

      try{
        
        File jsonFile = new File(pendingDataMessagesFilename);
        if (jsonFile.exists()) { jsonFile.delete(); }
        
      } 
      catch (Throwable e){ Log.e(TAG, "getPendingDataMessages - Exception", e); }  
         
    } finally {
      lock.unlock();
    }

    return jsonTxt;

  }

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

          boolean observerIsPresent;
          try{
            observerIsPresent = newMessageDispatcher.hasActiveObservers(); 
            if (observerIsPresent) { 
              Log.v(TAG, "onMessageReceived - observerIsPresent: true"); 
              newMessageDispatcher.setValue(jsonData.toString()); 
            }
            else {
              Log.v(TAG, "onMessageReceived - observerIsPresent: false");             
            }
          } catch (Throwable e){ 
            Log.e(TAG, "onMessageReceived - Exception", e); 
            observerIsPresent = false; 
          }  

          try{
        
            /* if no observer then save the message to let 
               the main activity retrieve it later */
            if (!observerIsPresent) { 

              lock.lock();
              try {

                /* load the json */
                String jsonTxt = getPendingDataMessagesInternal();
                JSONObject jsonRoot = new JSONObject(jsonTxt);       
                
                /* max 1000 items to not exagerate the size of the queue */
                JSONArray messagesNode = jsonRoot.optJSONArray("messages");
                if ((messagesNode != null) && (messagesNode.length() >= 1000) && (Build.VERSION.SDK_INT >= 19)) { messagesNode.remove(0); }
                if ((messagesNode == null) || (messagesNode.length() < 1000)) { 
      
                  /* update the json */
                  if (messagesNode == null) { 
                    messagesNode = new JSONArray();
                    jsonRoot.put("messages", messagesNode);
                  }
                  messagesNode.put(jsonData);
                  
                  /* save the json */
                  FileWriter fw = new FileWriter(pendingDataMessagesFilename);
                  fw.write(jsonRoot.toString());
                  fw.flush();
                  fw.close();
                
                }
              
              } finally {
                lock.unlock();
              }
              
            }
            
          } 
          catch (Throwable e){ Log.e(TAG, "onMessageReceived - Exception", e); }  

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
