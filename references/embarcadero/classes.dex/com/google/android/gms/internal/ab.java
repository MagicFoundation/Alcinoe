package com.google.android.gms.internal;

import android.content.Context;
import android.os.Parcel;
import android.util.DisplayMetrics;
import com.google.android.gms.ads.AdSize;
import com.google.android.gms.ads.a;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public final class ab implements SafeParcelable {
    public static final ac CREATOR;
    public final int height;
    public final int heightPixels;
    public final String ln;
    public final boolean lo;
    public final ab[] lp;
    public final int versionCode;
    public final int width;
    public final int widthPixels;

    static {
        CREATOR = new ac();
    }

    public ab() {
        this(2, "interstitial_mb", 0, 0, true, 0, 0, null);
    }

    ab(int i, String str, int i2, int i3, boolean z, int i4, int i5, ab[] abVarArr) {
        this.versionCode = i;
        this.ln = str;
        this.height = i2;
        this.heightPixels = i3;
        this.lo = z;
        this.width = i4;
        this.widthPixels = i5;
        this.lp = abVarArr;
    }

    public ab(Context context, AdSize adSize) {
        this(context, new AdSize[]{adSize});
    }

    public ab(Context context, AdSize[] adSizeArr) {
        int i;
        int i2;
        int i3 = 0;
        AdSize adSize = adSizeArr[0];
        this.versionCode = 2;
        this.lo = false;
        this.width = adSize.getWidth();
        this.height = adSize.getHeight();
        int i4 = this.width == -1 ? 1 : 0;
        int i5 = this.height == -2 ? 1 : 0;
        DisplayMetrics displayMetrics = context.getResources().getDisplayMetrics();
        if (i4 != 0) {
            this.widthPixels = a(displayMetrics);
            i = (int) (((float) this.widthPixels) / displayMetrics.density);
        } else {
            i2 = this.width;
            this.widthPixels = cz.a(displayMetrics, this.width);
            i = i2;
        }
        i2 = i5 != 0 ? c(displayMetrics) : this.height;
        this.heightPixels = cz.a(displayMetrics, i2);
        if (i4 == 0 && i5 == 0) {
            this.ln = adSize.toString();
        } else {
            this.ln = i + "x" + i2 + "_as";
        }
        if (adSizeArr.length > 1) {
            this.lp = new ab[adSizeArr.length];
            while (i3 < adSizeArr.length) {
                this.lp[i3] = new ab(context, adSizeArr[i3]);
                i3++;
            }
            return;
        }
        this.lp = null;
    }

    public ab(ab abVar, ab[] abVarArr) {
        this(2, abVar.ln, abVar.height, abVar.heightPixels, abVar.lo, abVar.width, abVar.widthPixels, abVarArr);
    }

    public static int a(DisplayMetrics displayMetrics) {
        return displayMetrics.widthPixels;
    }

    public static int b(DisplayMetrics displayMetrics) {
        return (int) (((float) c(displayMetrics)) * displayMetrics.density);
    }

    private static int c(DisplayMetrics displayMetrics) {
        int i = (int) (((float) displayMetrics.heightPixels) / displayMetrics.density);
        return i <= 400 ? 32 : i <= 720 ? 50 : 90;
    }

    public AdSize ai() {
        return a.a(this.width, this.height, this.ln);
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel out, int flags) {
        ac.a(this, out, flags);
    }
}
