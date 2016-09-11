package com.google.android.gms.tagmanager;

import android.app.IntentService;
import android.content.Context;
import android.content.Intent;
import com.google.analytics.tracking.android.ModelFields;
import com.google.android.gms.analytics.CampaignTrackingService;

public final class InstallReferrerService extends IntentService {
    CampaignTrackingService Vk;
    Context Vl;

    public InstallReferrerService() {
        super("InstallReferrerService");
    }

    public InstallReferrerService(String name) {
        super(name);
    }

    private void a(Context context, Intent intent) {
        if (this.Vk == null) {
            this.Vk = new CampaignTrackingService();
        }
        this.Vk.processIntent(context, intent);
    }

    protected void onHandleIntent(Intent intent) {
        String stringExtra = intent.getStringExtra(ModelFields.REFERRER);
        Context applicationContext = this.Vl != null ? this.Vl : getApplicationContext();
        ay.c(applicationContext, stringExtra);
        a(applicationContext, intent);
    }
}
