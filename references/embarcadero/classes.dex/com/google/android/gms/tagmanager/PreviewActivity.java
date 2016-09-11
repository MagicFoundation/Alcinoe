package com.google.android.gms.tagmanager;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.AlertDialog.Builder;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;

public class PreviewActivity extends Activity {
    private void a(String str, String str2, String str3) {
        AlertDialog create = new Builder(this).create();
        create.setTitle(str);
        create.setMessage(str2);
        create.setButton(-1, str3, new OnClickListener() {
            final /* synthetic */ PreviewActivity VR;

            {
                this.VR = r1;
            }

            public void onClick(DialogInterface dialog, int which) {
            }
        });
        create.show();
    }

    public void onCreate(Bundle savedInstanceState) {
        try {
            super.onCreate(savedInstanceState);
            bh.u("Preview activity");
            Uri data = getIntent().getData();
            if (!TagManager.getInstance(this).f(data)) {
                String str = "Cannot preview the app with the uri: " + data + ". Launching current version instead.";
                bh.w(str);
                a("Preview failure", str, "Continue");
            }
            Intent launchIntentForPackage = getPackageManager().getLaunchIntentForPackage(getPackageName());
            if (launchIntentForPackage != null) {
                bh.u("Invoke the launch activity for package name: " + getPackageName());
                startActivity(launchIntentForPackage);
                return;
            }
            bh.u("No launch activity found for package name: " + getPackageName());
        } catch (Exception e) {
            bh.t("Calling preview threw an exception: " + e.getMessage());
        }
    }
}
