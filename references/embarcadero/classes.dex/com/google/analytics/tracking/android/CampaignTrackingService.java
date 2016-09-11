package com.google.analytics.tracking.android;

import android.app.IntentService;
import android.content.Intent;
import java.io.IOException;
import java.io.OutputStream;

public final class CampaignTrackingService extends IntentService {
    public CampaignTrackingService(String name) {
        super(name);
    }

    public CampaignTrackingService() {
        super("CampaignIntentService");
    }

    protected void onHandleIntent(Intent intent) {
        String campaign = intent.getStringExtra(ModelFields.REFERRER);
        try {
            OutputStream output = openFileOutput("gaInstallData", 0);
            output.write(campaign.getBytes());
            output.close();
        } catch (IOException e) {
            Log.e("Error storing install campaign.");
        }
    }
}
