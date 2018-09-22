package com.alcinoe.firebase.messaging;

import java.util.concurrent.atomic.AtomicInteger;
import java.net.URL;
import java.net.HttpURLConnection;
import java.util.Map;
import java.io.FileWriter;
import java.io.File;
import java.io.FileInputStream;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import org.json.JSONObject;
import org.json.JSONArray;
import android.support.v4.content.LocalBroadcastManager;
import android.support.v4.app.NotificationCompat;
import android.content.Intent;
import android.app.PendingIntent;
import android.app.NotificationManager;
import android.content.Context;
import android.util.Log;
import android.net.Uri;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.media.RingtoneManager;
import android.os.Build;
import com.google.firebase.messaging.FirebaseMessagingService;
import com.google.firebase.messaging.RemoteMessage;
import me.leolin.shortcutbadger.ShortcutBadger;
import com.embarcadero.firemonkey.FMXNativeActivity;

public class ALFirebaseMessagingService extends FirebaseMessagingService {
        
  private static final String TAG = "ALFirebaseMessagingService";
  public static final String ACTION_MESSAGERECEIVED = "com.alcinoe.firebase.messaging.messageReceived";
  private final static Lock lock = new ReentrantLock(); 
  private static String pendingDataMessagesFilename = "";

  private static int currentTimeMillis() {
    return (int) (System.currentTimeMillis() % Integer.MAX_VALUE);
  }
  private final static AtomicInteger c = new AtomicInteger(currentTimeMillis());
  private static int getUniqueID() {
    return c.incrementAndGet();
  }
  
    
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
   * Called to process a message received
   * this procedure is public and static to let possible to call it from other service (for example)
   * @param remoteMessage Object representing the message received from Firebase Cloud Messaging.
   */
  public static void notify(Context context, RemoteMessage remoteMessage) {
        
    // There are two types of messages data messages and notification messages. Data messages are handled
    // here in onMessageReceived whether the app is in the foreground or background. Data messages are the type
    // traditionally used with GCM. Notification messages are only received here in onMessageReceived when the app
    // is in the foreground. When the app is in the background an automatically generated notification is displayed.
    // When the user taps on the notification they are returned to the app. Messages containing both notification
    // and data payloads are treated as notification messages. The Firebase console always sends notification
    // messages. For more see: https://firebase.google.com/docs/cloud-messaging/concept-options
    
    //Log
    Log.v(TAG, "onMessageReceived");
    
    //init data
    Map<String, String> data = remoteMessage.getData();
  
    /* build the intent */
    Intent intent = new Intent(ACTION_MESSAGERECEIVED);
    for (Map.Entry<String, String> entry : data.entrySet()) {
      intent.putExtra(entry.getKey(), entry.getValue()); /* String */
    }    
    intent.putExtra("gcm.from", remoteMessage.getFrom()); /* String */
    intent.putExtra("gcm.message_id", remoteMessage.getMessageId()); /* String */
    intent.putExtra("gcm.message_type", remoteMessage.getMessageType()); /* String */
    intent.putExtra("gcm.sent_time", remoteMessage.getSentTime()); /* long */
    intent.putExtra("gcm.to", remoteMessage.getTo()); /* String */
    intent.putExtra("gcm.ttl", remoteMessage.getTtl()); /* int */
    
    /* send the data to registered receivers */
    /* sendBroadcast() returns true if there was 1+ receivers, false otherwise */
    boolean receveirIsPresent;
    try{
  
      receveirIsPresent = LocalBroadcastManager.getInstance(context).sendBroadcast(intent);
  
    } catch (Throwable e){ 
      Log.e(TAG, "onMessageReceived - Exception", e); 
      receveirIsPresent = false; 
    }  
                   
  
    try{
  
      /* if no receveir and not a notification message */
      if ((!receveirIsPresent) && 
          (remoteMessage.getNotification() == null)) { 
      
        /* custom notification is present in data payload */
        if (data.containsKey("notification") && data.get("notification").equals("1")) {
              
          // actually i support these params, but nothing forbid to extends them
          // notification - Must be equal to 1 to activate showing of custom notification when no receiver
          // notification.channel - on Android 0 The notification will be posted on this NotificationChannel. 
          // notification.tag - A string identifier for this notification. 
          // notification.color - The accent color to use
          // notification.text - Set the second line of text in the platform notification template.
          // notification.title - Set the first line of text in the platform notification template.
          // notification.largeicon - url of the large icon to use - Add a large icon to the notification content view
          // notification.number - must equal to "auto" to increase the number of items this notification represents.
          // notification.onlyalertonce - Set this flag if you would only like the sound, vibrate and ticker to be played if the notification is not already showing.      
          // notification.smallicon - The name of the desired resource. - Set the small icon resource, which will be used to represent the notification in the status bar.
          // notification.ticker - Set the "ticker" text which is sent to accessibility services (The pop-up Text in Status Bar when the Notification is Called)
          // notification.vibrate - must equal to 1 to activate the default vibration pattern (0, 1200)
          // notification.visibility - Specify the value of visibility - One of VISIBILITY_PRIVATE (the default), VISIBILITY_SECRET, or VISIBILITY_PUBLIC.
          // notification.priority - Relative priority for this notification
          // notification.sound - Set the sound to play - use "default" for the default sound
          // notification.badgecount - update the shortcut badge count with this number 
                               
          int defaults = NotificationCompat.DEFAULT_LIGHTS;
          
          intent.putExtra("notification.presented", "1"); /* this to know in delphi code that it's a notification presented to the user (so if we receive mean he click it) */
          intent.setClass(context, FMXNativeActivity.class);
          intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
          PendingIntent pendingIntent = PendingIntent.getActivity(context, /* context	Context: The Context in which this PendingIntent should start the activity. */
                                                                  getUniqueID(), /* requestCode	int: Private request code for the sender */ 
                                                                  intent, /* intents	Intent: Array of Intents of the activities to be launched. */
                                                                  PendingIntent.FLAG_UPDATE_CURRENT); /* flags	int: May be FLAG_ONE_SHOT, - Flag indicating that this PendingIntent can be used only once. 
                                                                                                                            FLAG_NO_CREATE, - Flag indicating that if the described PendingIntent does not already exist, then simply return null instead of creating it.
                                                                                                                            FLAG_CANCEL_CURRENT, - Flag indicating that if the described PendingIntent already exists, the current one should be canceled before generating a new one. 
                                                                                                                            FLAG_UPDATE_CURRENT, - Flag indicating that if the described PendingIntent already exists, then keep it but replace its extra data with what is in this new Intent.
                                                                                                                            FLAG_IMMUTABLE - Flag indicating that the created PendingIntent should be immutable.
                                                                                                         or any of the flags as supported by Intent.fillIn() to 
                                                                                                         control which unspecified parts of the intent that can 
                                                                                                         be supplied when the actual send happens. */
          
          NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(context, data.get("notification.channel"));
          if (data.containsKey("notification.color")) { 
            notificationBuilder = notificationBuilder.setColor(Integer.parseInt(data.get("notification.color")));
          }
          if (data.containsKey("notification.text")) { 
            notificationBuilder = notificationBuilder.setContentText(data.get("notification.text"));
          }
          if (data.containsKey("notification.title")) { 
            notificationBuilder = notificationBuilder.setContentTitle(data.get("notification.title"));
          }
          if (data.containsKey("notification.largeicon")) { 
            try {
            
              //https://stackoverflow.com/questions/41067081/does-firebasemessagingservice-run-in-the-background-by-default
              //FirebaseMessagingService's method onMessageReceived(RemoteMessage message) is called "in the background" 
              //(not on the UI/Main thread). So all work that is done within onMessageReceived(RemoteMessage message) 
              //should be done synchronously because it's in its own background worker thread.
              
              URL url = new URL(data.get("notification.largeicon"));
              HttpURLConnection httpURLConnection = (HttpURLConnection) url.openConnection(); 
              httpURLConnection.setConnectTimeout(15000);
              httpURLConnection.setReadTimeout(15000);  
              Bitmap bitmap = BitmapFactory.decodeStream(httpURLConnection.getInputStream());
              if (bitmap != null) {
    
                int w;
                if (bitmap.getWidth() < bitmap.getHeight()) { w = bitmap.getWidth(); }
                else { w = bitmap.getHeight(); }
    
                Bitmap bitmapCropped = Bitmap.createBitmap(bitmap/*src*/, (bitmap.getWidth() - w) / 2/*X*/, (bitmap.getHeight() - w) / 2/*Y*/, w/*Width*/, w/*height*/, null/*m*/, true/*filter*/);
                if (!bitmap.sameAs(bitmapCropped)) { bitmap.recycle(); }
                
                notificationBuilder = notificationBuilder.setLargeIcon(bitmapCropped); 
    
              }
            
            } catch(Throwable e) { Log.e(TAG, "onMessageReceived - Exception", e); }
          }
          if (data.containsKey("notification.number") && 
              data.containsKey("notification.tag") && 
              data.get("notification.number").equals("auto")) { 
            SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(context.getApplicationContext());
            int currentCount = sp.getInt("notification.count_" + data.get("notification.tag"), 0);
            SharedPreferences.Editor editor = sp.edit();
            editor.putInt("notification.count_" + data.get("notification.tag"), currentCount + 1);
            editor.commit();
            if (currentCount > 0) { notificationBuilder = notificationBuilder.setNumber(currentCount + 1); }
          }
          else if (data.containsKey("notification.number")) {
            notificationBuilder = notificationBuilder.setNumber(Integer.parseInt(data.get("notification.number")));  
          }
          if (data.containsKey("notification.onlyalertonce") && data.get("notification.onlyalertonce").equals("1")) { 
            notificationBuilder = notificationBuilder.setOnlyAlertOnce(true);
          } 
          if (data.containsKey("notification.smallicon")) { 
            notificationBuilder = notificationBuilder.setSmallIcon(
              context.getApplicationContext().getResources().getIdentifier(
                data.get("notification.smallicon"), // name	String: The name of the desired resource.
                "drawable", // String: Optional default resource type to find, if "type/" is not included in the name. Can be null to require an explicit type.
                context.getApplicationContext().getPackageName())); // String: Optional default package to find, if "package:" is not included in the name. Can be null to require an explicit package.
          }                  
          if (data.containsKey("notification.ticker")) { 
            notificationBuilder = notificationBuilder.setTicker(data.get("notification.ticker"));
          }
          if (data.containsKey("notification.vibrate")) { 
            if (data.get("notification.vibrate").equals("default")) {
              defaults = defaults | NotificationCompat.DEFAULT_VIBRATE;
              notificationBuilder = notificationBuilder.setVibrate(new long[] { 0, 1200 });
            }
          } 
          if (data.containsKey("notification.visibility")) { 
            notificationBuilder = notificationBuilder.setVisibility(Integer.parseInt(data.get("notification.visibility")));
          }
          if (data.containsKey("notification.priority")) { 
            notificationBuilder = notificationBuilder.setPriority(Integer.parseInt(data.get("notification.priority")));
          }
          if (data.containsKey("notification.sound")) { 
            if (data.get("notification.sound").equals("default")) {
              defaults = defaults | NotificationCompat.DEFAULT_SOUND;
              notificationBuilder = notificationBuilder.setSound(RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION));
            }
            else {
              notificationBuilder = notificationBuilder.setSound(Uri.parse(data.get("notification.sound")));
            }
          }
          notificationBuilder = notificationBuilder.setDefaults(defaults);
          notificationBuilder = notificationBuilder.setWhen(System.currentTimeMillis());
          notificationBuilder = notificationBuilder.setShowWhen(true);
          notificationBuilder = notificationBuilder.setAutoCancel(true);
          notificationBuilder = notificationBuilder.setContentIntent(pendingIntent);
          
          if (data.containsKey("notification.badgecount")) { 
            ShortcutBadger.applyCount(context.getApplicationContext(), Integer.parseInt(data.get("notification.badgecount")));
          } 
      
          NotificationManager notificationManager = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);    
          notificationManager.notify(data.get("notification.tag"), /* tag	String: A string identifier for this notification. May be null. */ 
                                     0, /* id	int: An identifier for this notification. The pair (tag, id) must be unique within your application. */  
                                     notificationBuilder.build()); /* notification	Notification: A Notification object describing what to show the user. Must not be null. */  
        
        }
              
        /* NO custom notification is present in data payload */
        else if (!pendingDataMessagesFilename.equals("")) {
              
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
              JSONObject newNode = new JSONObject();
              for (Map.Entry<String, String> entry : data.entrySet()) {
                newNode.put(entry.getKey(), entry.getValue()); /* String */
              }    
              newNode.put("gcm.from", remoteMessage.getFrom()); /* String */
              newNode.put("gcm.message_id", remoteMessage.getMessageId()); /* String */
              newNode.put("gcm.message_type", remoteMessage.getMessageType()); /* String */
              newNode.put("gcm.sent_time", remoteMessage.getSentTime()); /* long */
              newNode.put("gcm.to", remoteMessage.getTo()); /* String */
              newNode.put("gcm.ttl", remoteMessage.getTtl()); /* int */
              if (messagesNode == null) { 
                messagesNode = new JSONArray();
                jsonRoot.put("messages", messagesNode);
              }
              messagesNode.put(newNode);
                      
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
    
    } 
    catch (Throwable e){ Log.e(TAG, "onMessageReceived - Exception", e); }  
        
  }

  /**
   * Called when message is received.
   * @param remoteMessage Object representing the message received from Firebase Cloud Messaging.
   */
  @Override
  public void onMessageReceived(RemoteMessage remoteMessage) {

    notify(this, remoteMessage);

  }

}
