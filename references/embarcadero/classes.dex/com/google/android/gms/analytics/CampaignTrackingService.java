package com.google.android.gms.analytics;

import android.app.IntentService;
import android.content.Context;
import android.content.Intent;
import com.google.analytics.tracking.android.ModelFields;
import java.io.IOException;
import java.io.OutputStream;

public class CampaignTrackingService extends IntentService {
    public CampaignTrackingService() {
        super("CampaignIntentService");
    }

    public CampaignTrackingService(String name) {
        super(name);
    }

    public void onHandleIntent(Intent intent) {
        processIntent(this, intent);
    }

    public void processIntent(Context context, Intent intent) {
        String stringExtra = intent.getStringExtra(ModelFields.REFERRER);
        try {
            OutputStream openFileOutput = context.openFileOutput("gaInstallData", 0);
            openFileOutput.write(stringExtra.getBytes());
            openFileOutput.close();
            aa.v("Stored campaign information.");
        } catch (IOException e) {
            aa.t("Error storing install campaign.");
        }
    }
}
