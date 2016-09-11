package com.google.android.gms.analytics;

import android.content.Context;
import android.util.DisplayMetrics;

class ae implements m {
    private static Object qI;
    private static ae uk;
    private final Context mContext;

    static {
        qI = new Object();
    }

    protected ae(Context context) {
        this.mContext = context;
    }

    public static ae cs() {
        ae aeVar;
        synchronized (qI) {
            aeVar = uk;
        }
        return aeVar;
    }

    public static void n(Context context) {
        synchronized (qI) {
            if (uk == null) {
                uk = new ae(context);
            }
        }
    }

    protected String ct() {
        DisplayMetrics displayMetrics = this.mContext.getResources().getDisplayMetrics();
        return displayMetrics.widthPixels + "x" + displayMetrics.heightPixels;
    }

    public String getValue(String field) {
        return (field != null && field.equals("&sr")) ? ct() : null;
    }

    public boolean x(String str) {
        return "&sr".equals(str);
    }
}
