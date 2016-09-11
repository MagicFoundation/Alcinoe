package com.embarcadero.gcm.notifications;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.support.v4.content.WakefulBroadcastReceiver;

public class GCMNotification extends WakefulBroadcastReceiver {
    private static GCMNativeListener mListener;

    public void setNativeListener(GCMNativeListener listener) {
        mListener = listener;
    }

    public void onReceive(Context context, Intent intent) {
        if (mListener == null) {
            WakefulBroadcastReceiver.startWakefulService(context, intent.setComponent(new ComponentName(context.getPackageName(), GCMIntentService.class.getName())));
        } else {
            Bundle extras = intent.getExtras();
            if (extras == null) {
                extras = new Bundle();
            }
            mListener.OnNotificationReceived(extras);
        }
        setResultCode(-1);
    }
}
