package com.embarcadero.gcm.notifications;

import android.app.IntentService;
import android.content.Intent;
import com.embarcadero.rtl.notifications.NotificationPublisher;

public class GCMIntentService extends IntentService {
    public GCMIntentService() {
        super("GCMIntentService");
    }

    protected void onHandleIntent(Intent intent) {
        new NotificationPublisher(this).publishGCM(intent.getExtras());
    }
}
