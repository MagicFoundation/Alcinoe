package com.alcinoe.content;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.os.Bundle;

public class ALStartServiceBroadcastReceiver extends BroadcastReceiver {
    
    @Override
    public void onReceive(Context context, Intent intent) {

        try {

            ApplicationInfo ai = context.getPackageManager().getApplicationInfo(context.getPackageName(), PackageManager.GET_META_DATA);
            Bundle bundle = ai.metaData;
            String serviceName = bundle.getString("com.alcinoe.startServiceName");

            Log.v("ALStartServiceBroadcastReceiver", serviceName);
            Intent startServiceIntent = new Intent();
            startServiceIntent.setClassName(context.getPackageName(), serviceName);
            context.startService(startServiceIntent);

        } catch (Throwable e){ Log.e("ALStartServiceBroadcastReceiver", "Exception", e); }  
        
    }
    
}