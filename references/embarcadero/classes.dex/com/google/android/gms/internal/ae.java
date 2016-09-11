package com.google.android.gms.internal;

import android.content.Context;
import android.content.res.TypedArray;
import android.text.TextUtils;
import android.util.AttributeSet;
import com.google.android.gms.R;
import com.google.android.gms.ads.AdSize;

public final class ae {
    private final AdSize[] lr;
    private final String ls;

    public ae(Context context, AttributeSet attributeSet) {
        int i = 1;
        TypedArray obtainAttributes = context.getResources().obtainAttributes(attributeSet, R.styleable.AdsAttrs);
        Object string = obtainAttributes.getString(0);
        Object string2 = obtainAttributes.getString(1);
        int i2 = !TextUtils.isEmpty(string) ? 1 : 0;
        if (TextUtils.isEmpty(string2)) {
            i = 0;
        }
        if (i2 != 0 && r1 == 0) {
            this.lr = f(string);
        } else if (i2 == 0 && r1 != 0) {
            this.lr = f(string2);
        } else if (i2 == 0 || r1 == 0) {
            throw new IllegalArgumentException("Required XML attribute \"adSize\" was missing.");
        } else {
            throw new IllegalArgumentException("Either XML attribute \"adSize\" or XML attribute \"supportedAdSizes\" should be specified, but not both.");
        }
        this.ls = obtainAttributes.getString(2);
        if (TextUtils.isEmpty(this.ls)) {
            throw new IllegalArgumentException("Required XML attribute \"adUnitId\" was missing.");
        }
    }

    private static AdSize[] f(String str) {
        String[] split = str.split("\\s*,\\s*");
        AdSize[] adSizeArr = new AdSize[split.length];
        for (int i = 0; i < split.length; i++) {
            String trim = split[i].trim();
            if (trim.matches("^(\\d+|FULL_WIDTH)\\s*[xX]\\s*(\\d+|AUTO_HEIGHT)$")) {
                String[] split2 = trim.split("[xX]");
                split2[0] = split2[0].trim();
                split2[1] = split2[1].trim();
                try {
                    adSizeArr[i] = new AdSize("FULL_WIDTH".equals(split2[0]) ? -1 : Integer.parseInt(split2[0]), "AUTO_HEIGHT".equals(split2[1]) ? -2 : Integer.parseInt(split2[1]));
                } catch (NumberFormatException e) {
                    throw new IllegalArgumentException("Could not parse XML attribute \"adSize\": " + trim);
                }
            } else if ("BANNER".equals(trim)) {
                adSizeArr[i] = AdSize.BANNER;
            } else if ("FULL_BANNER".equals(trim)) {
                adSizeArr[i] = AdSize.FULL_BANNER;
            } else if ("LEADERBOARD".equals(trim)) {
                adSizeArr[i] = AdSize.LEADERBOARD;
            } else if ("MEDIUM_RECTANGLE".equals(trim)) {
                adSizeArr[i] = AdSize.MEDIUM_RECTANGLE;
            } else if ("SMART_BANNER".equals(trim)) {
                adSizeArr[i] = AdSize.SMART_BANNER;
            } else if ("WIDE_SKYSCRAPER".equals(trim)) {
                adSizeArr[i] = AdSize.WIDE_SKYSCRAPER;
            } else {
                throw new IllegalArgumentException("Could not parse XML attribute \"adSize\": " + trim);
            }
        }
        if (adSizeArr.length != 0) {
            return adSizeArr;
        }
        throw new IllegalArgumentException("Could not parse XML attribute \"adSize\": " + str);
    }

    public AdSize[] c(boolean z) {
        if (z || this.lr.length == 1) {
            return this.lr;
        }
        throw new IllegalArgumentException("The adSizes XML attribute is only allowed on PublisherAdViews.");
    }

    public String getAdUnitId() {
        return this.ls;
    }
}
