package io.magicfoundation.alcinoe.broadcastreceiver;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.util.Log;
import org.json.JSONArray;

public class ALBroadcastReceiver extends BroadcastReceiver {

  private static final String TAG = "ALBroadcastReceiver";
  private static final String PREFERENCES_NAME  = "io.magicfoundation.alcinoe.broadcastreceiver.pending_broadcasts";
  private static final String KEY_INTENTS = "intents";

  private static ALBroadcastReceiverListener sListener;

  public static void setListener(ALBroadcastReceiverListener listener) {
    sListener = listener;
  }
  
  public static void deliverPendingBroadcasts(Context context) {
    if (context != null && sListener != null) {
      try {
        SharedPreferences prefs = context.getSharedPreferences(PREFERENCES_NAME, Context.MODE_PRIVATE);
        String data = prefs.getString(KEY_INTENTS, null/*defValue*/);
        // Intentionally clear pending intents first. If parsing or delivery 
        // fails we lose them, but we guarantee no infinite retry loop.
        prefs.edit().remove(KEY_INTENTS).apply();
        if (data == null || data.isEmpty()) return;
        JSONArray array = new JSONArray(data);
        for (int i = 0; i < array.length(); i++) {
          String uri = array.optString(i, null);
          if (uri == null || uri.isEmpty()) continue;
          Intent intent = Intent.parseUri(uri, Intent.URI_INTENT_SCHEME);
          sListener.onReceive(context, intent);
        }
      } catch (Exception e) {
        Log.w(TAG, "Failed to deliver pending broadcasts", e);
      }
    }
  }

  @Override
  public void onReceive(Context context, Intent intent) {
    if (sListener != null) sListener.onReceive(context, intent);
    else if (context != null && intent != null) {
      String uri = intent.toUri(Intent.URI_INTENT_SCHEME);
      SharedPreferences prefs = null;
      try {
        prefs = context.getSharedPreferences(PREFERENCES_NAME, Context.MODE_PRIVATE);
        String existing = prefs.getString(KEY_INTENTS, null);
        JSONArray array;
        if (existing != null && !existing.isEmpty()) array = new JSONArray(existing);
        else array = new JSONArray();
        array.put(uri);
        prefs.edit().putString(KEY_INTENTS, array.toString()).apply();
      } catch (Exception e) {
        Log.w(TAG, "Failed to persist broadcast Intent", e);
        if (prefs != null) prefs.edit().remove(KEY_INTENTS).apply();
      }        
    }
  }

}