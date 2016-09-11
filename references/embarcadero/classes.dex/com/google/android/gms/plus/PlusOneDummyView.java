package com.google.android.gms.plus;

import android.content.Context;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.res.Resources;
import android.graphics.Point;
import android.graphics.drawable.Drawable;
import android.util.DisplayMetrics;
import android.util.TypedValue;
import android.view.View;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.FrameLayout.LayoutParams;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class PlusOneDummyView extends FrameLayout {
    public static final String TAG = "PlusOneDummyView";

    private interface d {
        Drawable getDrawable(int i);

        boolean isValid();
    }

    private static class a implements d {
        private Context mContext;

        private a(Context context) {
            this.mContext = context;
        }

        public Drawable getDrawable(int size) {
            return this.mContext.getResources().getDrawable(17301508);
        }

        public boolean isValid() {
            return true;
        }
    }

    private static class b implements d {
        private Context mContext;

        private b(Context context) {
            this.mContext = context;
        }

        public Drawable getDrawable(int size) {
            try {
                String str;
                Resources resources = this.mContext.createPackageContext(GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_PACKAGE, 4).getResources();
                String str2 = GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_PACKAGE;
                switch (size) {
                    case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                        str = "ic_plusone_small";
                        break;
                    case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                        str = "ic_plusone_medium";
                        break;
                    case DetectedActivity.ON_FOOT /*2*/:
                        str = "ic_plusone_tall";
                        break;
                    default:
                        str = "ic_plusone_standard";
                        break;
                }
                return resources.getDrawable(resources.getIdentifier(str, "drawable", str2));
            } catch (NameNotFoundException e) {
                return null;
            }
        }

        public boolean isValid() {
            try {
                this.mContext.createPackageContext(GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_PACKAGE, 4).getResources();
                return true;
            } catch (NameNotFoundException e) {
                return false;
            }
        }
    }

    private static class c implements d {
        private Context mContext;

        private c(Context context) {
            this.mContext = context;
        }

        public Drawable getDrawable(int size) {
            String str;
            switch (size) {
                case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                    str = "ic_plusone_small_off_client";
                    break;
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    str = "ic_plusone_medium_off_client";
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    str = "ic_plusone_tall_off_client";
                    break;
                default:
                    str = "ic_plusone_standard_off_client";
                    break;
            }
            return this.mContext.getResources().getDrawable(this.mContext.getResources().getIdentifier(str, "drawable", this.mContext.getPackageName()));
        }

        public boolean isValid() {
            return (this.mContext.getResources().getIdentifier("ic_plusone_small_off_client", "drawable", this.mContext.getPackageName()) == 0 || this.mContext.getResources().getIdentifier("ic_plusone_medium_off_client", "drawable", this.mContext.getPackageName()) == 0 || this.mContext.getResources().getIdentifier("ic_plusone_tall_off_client", "drawable", this.mContext.getPackageName()) == 0 || this.mContext.getResources().getIdentifier("ic_plusone_standard_off_client", "drawable", this.mContext.getPackageName()) == 0) ? false : true;
        }
    }

    public PlusOneDummyView(Context context, int size) {
        super(context);
        View button = new Button(context);
        button.setEnabled(false);
        button.setBackgroundDrawable(hk().getDrawable(size));
        Point bA = bA(size);
        addView(button, new LayoutParams(bA.x, bA.y, 17));
    }

    private Point bA(int i) {
        int i2 = 24;
        int i3 = 20;
        Point point = new Point();
        switch (i) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                i3 = 14;
                break;
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                i2 = 32;
                break;
            case DetectedActivity.ON_FOOT /*2*/:
                i2 = 50;
                break;
            default:
                i2 = 38;
                i3 = 24;
                break;
        }
        DisplayMetrics displayMetrics = getResources().getDisplayMetrics();
        float applyDimension = TypedValue.applyDimension(1, (float) i2, displayMetrics);
        float applyDimension2 = TypedValue.applyDimension(1, (float) i3, displayMetrics);
        point.x = (int) (((double) applyDimension) + 0.5d);
        point.y = (int) (((double) applyDimension2) + 0.5d);
        return point;
    }

    private d hk() {
        d bVar = new b(null);
        if (!bVar.isValid()) {
            bVar = new c(null);
        }
        return !bVar.isValid() ? new a(null) : bVar;
    }
}
