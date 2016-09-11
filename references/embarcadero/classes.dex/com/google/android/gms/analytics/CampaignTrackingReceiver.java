package com.google.android.gms.analytics;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import com.google.analytics.tracking.android.ModelFields;

public final class CampaignTrackingReceiver extends BroadcastReceiver {
    public void onReceive(Context ctx, Intent intent) {
        String stringExtra = intent.getStringExtra(ModelFields.REFERRER);
        if ("com.android.vending.INSTALL_REFERRER".equals(intent.getAction()) && stringExtra != null) {
            Intent intent2 = new Intent(ctx, CampaignTrackingService.class);
            intent2.putExtra(ModelFields.REFERRER, stringExtra);
            ctx.startService(intent2);
        }
    }
}
