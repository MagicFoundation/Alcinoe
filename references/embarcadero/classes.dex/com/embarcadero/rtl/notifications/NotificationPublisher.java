package com.embarcadero.rtl.notifications;

import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.media.RingtoneManager;
import android.os.Bundle;
import android.support.v4.app.NotificationCompat.Builder;
import com.google.android.gms.gcm.GoogleCloudMessaging;
import com.google.android.gms.plus.PlusShare;
import org.json.JSONException;
import org.json.JSONObject;

public class NotificationPublisher {
    public static final String ACTION_GCM_NOTIFICATION = "GCMNotification";
    private static int mGCMID;
    Context mContext;

    static {
        mGCMID = 1;
    }

    public NotificationPublisher(Context context) {
        this.mContext = context;
    }

    public void publishGCM(Bundle extras) {
        String lastTitle = "";
        String lastMessage = "";
        if (extras != null) {
            for (String key : extras.keySet()) {
                String keystr = key;
                String valstr = extras.get(key).toString();
                if (lastMessage == "" || lastTitle == "") {
                    String msg = "";
                    String title = "";
                    if (valstr.startsWith("{") && valstr.endsWith("}")) {
                        JSONObject jsonVal;
                        try {
                            jsonVal = new JSONObject(valstr);
                        } catch (JSONException e) {
                            jsonVal = null;
                        }
                        if (jsonVal != null) {
                            if (jsonVal.has("message")) {
                                msg = jsonVal.optString("message");
                            } else if (jsonVal.has("msg")) {
                                msg = jsonVal.optString("msg");
                            } else if (jsonVal.has("alert")) {
                                msg = jsonVal.optString("alert");
                            }
                            if (jsonVal.has(PlusShare.KEY_CONTENT_DEEP_LINK_METADATA_TITLE)) {
                                title = jsonVal.optString(PlusShare.KEY_CONTENT_DEEP_LINK_METADATA_TITLE);
                            }
                        }
                    } else {
                        if (key.equals("message")) {
                            msg = valstr;
                        } else if (key.equals("msg")) {
                            msg = valstr;
                        }
                        if (key.equals(PlusShare.KEY_CONTENT_DEEP_LINK_METADATA_TITLE)) {
                            title = valstr;
                        }
                    }
                    if (lastMessage == "" && msg != "") {
                        lastMessage = msg;
                    }
                    if (lastTitle == "" && title != "") {
                        lastTitle = title;
                    }
                }
            }
        }
        publishGCM(lastTitle, lastMessage, extras);
    }

    private void publishGCM(String msgTitle, String msgText, Bundle gcmExtras) {
        if (msgTitle == "") {
            Context applicationContext = this.mContext.getApplicationContext();
            msgTitle = (String) applicationContext.getPackageManager().getApplicationLabel(applicationContext.getApplicationInfo());
        }
        Intent newIntent = this.mContext.getPackageManager().getLaunchIntentForPackage(this.mContext.getPackageName());
        newIntent.setAction(ACTION_GCM_NOTIFICATION);
        newIntent.setFlags(603979776);
        String str = GoogleCloudMessaging.MESSAGE_TYPE_MESSAGE;
        if (gcmExtras == null) {
            gcmExtras = new Bundle();
        }
        newIntent.putExtra(str, gcmExtras);
        Context context = this.mContext;
        int i = mGCMID;
        mGCMID = i + 1;
        Builder builder = buildNotification(msgTitle, msgText, PendingIntent.getActivity(context, i, newIntent, 134217728));
        NotificationManager notificationManager = (NotificationManager) this.mContext.getSystemService("notification");
        int i2 = mGCMID;
        mGCMID = i2 + 1;
        notificationManager.notify(i2, builder.build());
    }

    private Builder buildNotification(String msgTitle, String msgText, PendingIntent contentIntent) {
        int icon = this.mContext.getApplicationContext().getApplicationInfo().icon;
        Builder mBuilder = new Builder(this.mContext);
        mBuilder.setSmallIcon(icon);
        mBuilder.setTicker(msgTitle);
        mBuilder.setContentTitle(msgTitle);
        mBuilder.setContentText(msgText);
        mBuilder.setContentIntent(contentIntent);
        mBuilder.setSound(RingtoneManager.getDefaultUri(2));
        return mBuilder;
    }
}
