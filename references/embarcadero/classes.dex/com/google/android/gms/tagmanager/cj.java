package com.google.android.gms.tagmanager;

import android.content.Context;
import android.util.DisplayMetrics;
import android.view.WindowManager;
import com.google.android.gms.internal.a;
import com.google.android.gms.internal.d;
import java.util.Map;

class cj extends aj {
    private static final String ID;
    private final Context mContext;

    static {
        ID = a.RESOLUTION.toString();
    }

    public cj(Context context) {
        super(ID, new String[0]);
        this.mContext = context;
    }

    public boolean iy() {
        return true;
    }

    public d.a u(Map<String, d.a> map) {
        DisplayMetrics displayMetrics = new DisplayMetrics();
        ((WindowManager) this.mContext.getSystemService("window")).getDefaultDisplay().getMetrics(displayMetrics);
        int i = displayMetrics.widthPixels;
        return di.r(i + "x" + displayMetrics.heightPixels);
    }
}
