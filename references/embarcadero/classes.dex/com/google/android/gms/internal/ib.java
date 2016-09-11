package com.google.android.gms.internal;

import android.content.Context;
import android.content.pm.PackageManager.NameNotFoundException;
import com.google.android.gms.R;
import java.util.LinkedHashSet;
import java.util.Locale;

public class ib {
    private static final String TAG;
    private final hl<hg> Lk;
    private final LinkedHashSet<String> OA;
    private final Locale Or;
    private final ic Os;
    private final id Oz;
    private final Context mContext;

    static {
        TAG = ib.class.getSimpleName();
    }

    public ib(Context context, Locale locale, hl<hg> hlVar) {
        int i;
        this.mContext = context;
        this.Lk = hlVar;
        this.Or = locale;
        this.Oz = new id(context, locale);
        this.OA = new LinkedHashSet();
        this.OA.add(context.getString(R.string.location_client_powered_by_google));
        String packageName = this.mContext.getPackageName();
        try {
            i = this.mContext.getPackageManager().getPackageInfo(packageName, 0).versionCode;
        } catch (NameNotFoundException e) {
            i = -1;
        }
        this.Os = new ic(this.Lk, packageName, i);
    }
}
