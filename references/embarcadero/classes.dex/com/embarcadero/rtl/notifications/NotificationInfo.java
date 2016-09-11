package com.embarcadero.rtl.notifications;

import android.app.Notification;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.media.RingtoneManager;
import android.net.Uri;
import android.support.v4.app.NotificationCompat.Builder;
import java.io.File;

public class NotificationInfo {
    public static final String ACTION_NOTIFICATION = "ACTION_NOTIFICATION";
    public static final String EXTRA_ACTIVITY_CLASS_NAME = "EXTRA_ACTIVITY_CLASS_NAME";
    public static final String EXTRA_ALERT_ACTION = "EXTRA_ALERT_ACTION";
    public static final String EXTRA_ALERT_BODY = "EXTRA_ALERT_BODY";
    public static final String EXTRA_ENABLE_SOUND = "EXTRA_ENABLE_SOUND";
    public static final String EXTRA_FIRE_DATE = "EXTRA_FIRE_DATE";
    public static final String EXTRA_FIRE_GMT_DATE = "EXTRA_FIRE_GMT_DATE";
    public static final String EXTRA_HAS_ACTION = "EXTRA_HAS_ACTION";
    public static final String EXTRA_NAME = "EXTRA_NAME";
    public static final String EXTRA_NUMBER = "EXTRA_NUMBER";
    public static final String EXTRA_REPEAT_INTERVAL = "EXTRA_REPEAT_INTERVAL";
    public static final String EXTRA_SOUND_NAME = "EXTRA_SOUND_NAME";
    public static final String EXTRA_TITLE = "EXTRA_TITLE";
    public static final String EXTRA_UNIQUE_ID = "EXTRA_UNIQUE_ID";
    private Class<?> mActivityClass;
    private String mAlertAction;
    private String mAlertBody;
    private int mAppIcon;
    private Context mContext;
    private long mDate;
    private boolean mEnableSound;
    private boolean mHasAction;
    private Intent mIntent;
    private int mIntentCode;
    private String mName;
    private int mNotifyNumber;
    private int mRepeatInterval;
    private String mSoundName;
    private CharSequence mTitle;

    NotificationInfo(Context context, Intent intent) throws ClassNotFoundException {
        this.mContext = null;
        this.mActivityClass = null;
        this.mIntentCode = -1;
        this.mName = null;
        this.mTitle = null;
        this.mAlertBody = null;
        this.mAlertAction = null;
        this.mDate = System.currentTimeMillis();
        this.mRepeatInterval = 0;
        this.mNotifyNumber = 0;
        this.mEnableSound = false;
        this.mSoundName = null;
        this.mHasAction = false;
        this.mAppIcon = 0;
        this.mIntent = null;
        this.mActivityClass = Class.forName(intent.getStringExtra(EXTRA_ACTIVITY_CLASS_NAME));
        this.mContext = context;
        this.mIntent = intent;
        this.mIntentCode = intent.getIntExtra(EXTRA_UNIQUE_ID, -1);
        this.mName = intent.getStringExtra(EXTRA_NAME);
        this.mTitle = intent.getStringExtra(EXTRA_TITLE);
        this.mAlertBody = intent.getStringExtra(EXTRA_ALERT_BODY);
        this.mAlertAction = intent.getStringExtra(EXTRA_ALERT_ACTION);
        this.mNotifyNumber = intent.getIntExtra(EXTRA_NUMBER, 0);
        this.mEnableSound = intent.getBooleanExtra(EXTRA_ENABLE_SOUND, true);
        this.mSoundName = intent.getStringExtra(EXTRA_SOUND_NAME);
        this.mHasAction = false;
        this.mDate = intent.getLongExtra(EXTRA_FIRE_GMT_DATE, this.mDate);
        this.mRepeatInterval = intent.getIntExtra(EXTRA_REPEAT_INTERVAL, 0);
        this.mAppIcon = this.mContext.getApplicationInfo().icon;
    }

    Notification createNotification() {
        Builder mBuilder = new Builder(this.mContext);
        mBuilder.setDefaults(4);
        mBuilder.setSmallIcon(this.mAppIcon);
        mBuilder.setTicker(this.mTitle);
        mBuilder.setContentTitle(this.mTitle);
        mBuilder.setContentText(this.mAlertBody);
        mBuilder.setNumber(this.mNotifyNumber);
        mBuilder.setContentIntent(getContentIntent(this.mIntentCode));
        mBuilder.setAutoCancel(true);
        if (this.mHasAction) {
            mBuilder.addAction(17301560, this.mAlertAction, getActionIntent(1));
            mBuilder.addAction(17301591, "Cancel", getActionIntent(2));
        }
        if (this.mEnableSound) {
            if (this.mSoundName == null || this.mSoundName.isEmpty()) {
                mBuilder.setSound(RingtoneManager.getDefaultUri(2));
            } else {
                mBuilder.setSound(Uri.fromFile(new File(this.mSoundName)));
            }
        }
        return mBuilder.build();
    }

    public String getNotificationPreferencesValue() {
        return this.mName + "=" + this.mIntentCode;
    }

    public int getRepeatInterval() {
        return this.mRepeatInterval;
    }

    public int getIntentCode() {
        return this.mIntentCode;
    }

    public String getName() {
        return this.mName;
    }

    private PendingIntent getContentIntent(int code) {
        Intent intent = new Intent(this.mIntent);
        intent.setAction(ACTION_NOTIFICATION);
        intent.setClass(this.mContext, this.mActivityClass);
        intent.setFlags(603979776);
        return PendingIntent.getActivity(this.mContext, code, intent, 134217728);
    }

    private PendingIntent getActionIntent(int code) {
        return PendingIntent.getBroadcast(this.mContext, code, new Intent(this.mContext, this.mActivityClass), 134217728);
    }

    public String toString() {
        return "mName=" + this.mName + ", mIntentCode=" + this.mIntentCode + ", mTitle=" + this.mTitle + ", mAlertBody=" + this.mAlertBody + ", mAlertAction=" + this.mAlertAction + ", mDate=" + this.mDate + ", mRepeatInterval=" + this.mRepeatInterval + ", mNotifyNumber=" + this.mNotifyNumber + ", mEnableSound=" + this.mEnableSound + ", mSoundName=" + this.mSoundName + ", mHasAction=" + this.mHasAction + ", mAppIcon=" + this.mAppIcon;
    }
}
