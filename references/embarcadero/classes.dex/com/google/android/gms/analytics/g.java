package com.google.android.gms.analytics;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;

class g implements m {
    private static Object qI;
    private static g qV;
    protected String qR;
    protected String qS;
    protected String qT;
    protected String qU;

    static {
        qI = new Object();
    }

    protected g() {
    }

    private g(Context context) {
        PackageManager packageManager = context.getPackageManager();
        this.qT = context.getPackageName();
        this.qU = packageManager.getInstallerPackageName(this.qT);
        String str = this.qT;
        String str2 = null;
        try {
            PackageInfo packageInfo = packageManager.getPackageInfo(context.getPackageName(), 0);
            if (packageInfo != null) {
                str = packageManager.getApplicationLabel(packageInfo.applicationInfo).toString();
                str2 = packageInfo.versionName;
            }
        } catch (NameNotFoundException e) {
            aa.t("Error retrieving package info: appName set to " + str);
        }
        this.qR = str;
        this.qS = str2;
    }

    public static g bt() {
        return qV;
    }

    public static void n(Context context) {
        synchronized (qI) {
            if (qV == null) {
                qV = new g(context);
            }
        }
    }

    public String getValue(String field) {
        if (field == null) {
            return null;
        }
        if (field.equals("&an")) {
            return this.qR;
        }
        if (field.equals("&av")) {
            return this.qS;
        }
        if (field.equals("&aid")) {
            return this.qT;
        }
        return field.equals("&aiid") ? this.qU : null;
    }

    public boolean x(String str) {
        return "&an".equals(str) || "&av".equals(str) || "&aid".equals(str) || "&aiid".equals(str);
    }
}
