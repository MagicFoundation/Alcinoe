package com.google.android.gms.internal;

import android.app.Activity;
import android.content.ActivityNotFoundException;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.content.Intent;
import android.util.Log;

public class ef implements OnClickListener {
    private final int Bq;
    private final Intent mIntent;
    private final Activity nd;

    public ef(Activity activity, Intent intent, int i) {
        this.nd = activity;
        this.mIntent = intent;
        this.Bq = i;
    }

    public void onClick(DialogInterface dialog, int which) {
        try {
            if (this.mIntent != null) {
                this.nd.startActivityForResult(this.mIntent, this.Bq);
            }
            dialog.dismiss();
        } catch (ActivityNotFoundException e) {
            Log.e("SettingsRedirect", "Can't redirect to app settings for Google Play services");
        }
    }
}
