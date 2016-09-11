package com.google.android.gms.internal;

import android.content.Intent;
import android.net.Uri;
import android.support.v4.view.accessibility.AccessibilityEventCompat;
import com.google.android.gms.common.GooglePlayServicesUtil;

public class ek {
    private static final Uri Cb;
    private static final Uri Cc;

    static {
        Cb = Uri.parse("http://plus.google.com/");
        Cc = Cb.buildUpon().appendPath("circles").appendPath("find").build();
    }

    public static Intent af(String str) {
        Uri fromParts = Uri.fromParts("package", str, null);
        Intent intent = new Intent("android.settings.APPLICATION_DETAILS_SETTINGS");
        intent.setData(fromParts);
        return intent;
    }

    private static Uri ag(String str) {
        return Uri.parse("market://details").buildUpon().appendQueryParameter("id", str).build();
    }

    public static Intent ah(String str) {
        Intent intent = new Intent("android.intent.action.VIEW");
        intent.setData(ag(str));
        intent.setPackage(GooglePlayServicesUtil.GOOGLE_PLAY_STORE_PACKAGE);
        intent.addFlags(AccessibilityEventCompat.TYPE_GESTURE_DETECTION_END);
        return intent;
    }

    public static Intent ai(String str) {
        Uri parse = Uri.parse("bazaar://search?q=pname:" + str);
        Intent intent = new Intent("android.intent.action.VIEW");
        intent.setData(parse);
        intent.setFlags(AccessibilityEventCompat.TYPE_GESTURE_DETECTION_END);
        return intent;
    }

    public static Intent eh() {
        return new Intent("android.settings.DATE_SETTINGS");
    }
}
