package com.embarcadero.rtl.notifications;

import android.app.AlarmManager;
import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;

public class NotificationAlarm extends BroadcastReceiver {
    public static final String NOTIFICATION_CENTER = "NOTIFICATION_EMBT_CENTER";
    public static final String SETTINGS_NOTIFICATION_IDS = "SETTINGS_NOTIFICATION_IDS";
    private Context mContext;
    private NotificationInfo mNotification;

    public NotificationAlarm() {
        this.mNotification = null;
        this.mContext = null;
    }

    public void onReceive(Context context, Intent intent) {
        if (NotificationInfo.ACTION_NOTIFICATION.equals(intent.getAction())) {
            try {
                this.mContext = context;
                this.mNotification = new NotificationInfo(context, intent);
                presentNotification(this.mNotification);
                if (this.mNotification.getRepeatInterval() != 0) {
                    ((AlarmManager) context.getSystemService("alarm")).set(0, RepeatInterval.getRepeatIntervalMSsec(this.mNotification.getRepeatInterval()), PendingIntent.getBroadcast(context, this.mNotification.getIntentCode(), intent, 134217728));
                    return;
                }
                removeNotificationInfoFromSharedPreferences();
            } catch (ClassNotFoundException e) {
            }
        }
    }

    private void presentNotification(NotificationInfo notification) {
        Notification notify = notification.createNotification();
        NotificationManager mNotificationManager = (NotificationManager) this.mContext.getSystemService("notification");
        String notificationName = notification.getName();
        if (notificationName == null || notificationName.isEmpty()) {
            mNotificationManager.notify(notification.getIntentCode(), notify);
        } else {
            mNotificationManager.notify(notificationName, 0, notify);
        }
    }

    private void removeNotificationInfoFromSharedPreferences() {
        SharedPreferences preference = this.mContext.getSharedPreferences(NOTIFICATION_CENTER, 0);
        String notificationsString = preference.getString(SETTINGS_NOTIFICATION_IDS, null);
        if (notificationsString != null) {
            String[] notifications = notificationsString.split(System.getProperty("line.separator"));
            StringBuilder sb = new StringBuilder();
            for (String notification : notifications) {
                if (!notification.equals(this.mNotification.getNotificationPreferencesValue())) {
                    sb.append(notification);
                }
            }
            Editor editor = preference.edit();
            try {
                editor.putString(SETTINGS_NOTIFICATION_IDS, sb.toString());
            } finally {
                editor.commit();
            }
        }
    }
}
