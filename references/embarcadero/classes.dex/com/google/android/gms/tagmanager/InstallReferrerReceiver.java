package com.google.android.gms.tagmanager;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import com.google.analytics.tracking.android.ModelFields;

public final class InstallReferrerReceiver extends BroadcastReceiver {
    public void onReceive(Context context, Intent intent) {
        String stringExtra = intent.getStringExtra(ModelFields.REFERRER);
        if ("com.android.vending.INSTALL_REFERRER".equals(intent.getAction()) && stringExtra != null) {
            ay.bq(stringExtra);
            Intent intent2 = new Intent(context, InstallReferrerService.class);
            intent2.putExtra(ModelFields.REFERRER, stringExtra);
            context.startService(intent2);
        }
    }
}
