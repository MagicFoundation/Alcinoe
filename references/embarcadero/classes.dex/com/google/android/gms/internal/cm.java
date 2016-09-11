package com.google.android.gms.internal;

import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.pm.ResolveInfo;
import android.media.AudioManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.Uri;
import android.telephony.TelephonyManager;
import android.util.DisplayMetrics;
import com.google.android.gms.cast.Cast;
import java.util.Locale;

public final class cm {
    public final int oM;
    public final boolean oN;
    public final boolean oO;
    public final String oP;
    public final String oQ;
    public final boolean oR;
    public final boolean oS;
    public final boolean oT;
    public final String oU;
    public final String oV;
    public final int oW;
    public final int oX;
    public final int oY;
    public final int oZ;
    public final int pa;
    public final int pb;
    public final float pc;
    public final int pd;
    public final int pe;

    public cm(Context context) {
        boolean z = true;
        AudioManager audioManager = (AudioManager) context.getSystemService("audio");
        ConnectivityManager connectivityManager = (ConnectivityManager) context.getSystemService("connectivity");
        DisplayMetrics displayMetrics = context.getResources().getDisplayMetrics();
        Locale locale = Locale.getDefault();
        PackageManager packageManager = context.getPackageManager();
        TelephonyManager telephonyManager = (TelephonyManager) context.getSystemService("phone");
        this.oM = audioManager.getMode();
        this.oN = a(packageManager, "geo:0,0?q=donuts") != null;
        if (a(packageManager, "http://www.google.com") == null) {
            z = false;
        }
        this.oO = z;
        this.oP = telephonyManager.getNetworkOperator();
        this.oQ = locale.getCountry();
        this.oR = cz.aW();
        this.oS = audioManager.isMusicActive();
        this.oT = audioManager.isSpeakerphoneOn();
        this.oU = locale.getLanguage();
        this.oV = a(packageManager);
        this.oW = audioManager.getStreamVolume(3);
        this.oX = a(context, connectivityManager, packageManager);
        this.oY = telephonyManager.getNetworkType();
        this.oZ = telephonyManager.getPhoneType();
        this.pa = audioManager.getRingerMode();
        this.pb = audioManager.getStreamVolume(2);
        this.pc = displayMetrics.density;
        this.pd = displayMetrics.widthPixels;
        this.pe = displayMetrics.heightPixels;
    }

    private static int a(Context context, ConnectivityManager connectivityManager, PackageManager packageManager) {
        if (!cv.a(packageManager, context.getPackageName(), "android.permission.ACCESS_NETWORK_STATE")) {
            return -2;
        }
        NetworkInfo activeNetworkInfo = connectivityManager.getActiveNetworkInfo();
        return activeNetworkInfo != null ? activeNetworkInfo.getType() : -1;
    }

    private static ResolveInfo a(PackageManager packageManager, String str) {
        return packageManager.resolveActivity(new Intent("android.intent.action.VIEW", Uri.parse(str)), Cast.MAX_MESSAGE_LENGTH);
    }

    private static String a(PackageManager packageManager) {
        String str = null;
        ResolveInfo a = a(packageManager, "market://details?id=com.google.android.gms.ads");
        if (a != null) {
            ActivityInfo activityInfo = a.activityInfo;
            if (activityInfo != null) {
                try {
                    PackageInfo packageInfo = packageManager.getPackageInfo(activityInfo.packageName, 0);
                    if (packageInfo != null) {
                        str = packageInfo.versionCode + "." + activityInfo.packageName;
                    }
                } catch (NameNotFoundException e) {
                }
            }
        }
        return str;
    }
}
