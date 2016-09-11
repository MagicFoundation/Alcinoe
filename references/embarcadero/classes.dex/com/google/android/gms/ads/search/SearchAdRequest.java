package com.google.android.gms.ads.search;

import android.content.Context;
import android.graphics.Color;
import android.location.Location;
import com.google.android.gms.ads.mediation.NetworkExtras;
import com.google.android.gms.internal.aj;
import com.google.android.gms.internal.aj.a;

public final class SearchAdRequest {
    public static final int BORDER_TYPE_DASHED = 1;
    public static final int BORDER_TYPE_DOTTED = 2;
    public static final int BORDER_TYPE_NONE = 0;
    public static final int BORDER_TYPE_SOLID = 3;
    public static final int CALL_BUTTON_COLOR_DARK = 2;
    public static final int CALL_BUTTON_COLOR_LIGHT = 0;
    public static final int CALL_BUTTON_COLOR_MEDIUM = 1;
    public static final String DEVICE_ID_EMULATOR;
    public static final int ERROR_CODE_INTERNAL_ERROR = 0;
    public static final int ERROR_CODE_INVALID_REQUEST = 1;
    public static final int ERROR_CODE_NETWORK_ERROR = 2;
    public static final int ERROR_CODE_NO_FILL = 3;
    private final aj kA;
    private final int qA;
    private final int qB;
    private final String qC;
    private final int qD;
    private final String qE;
    private final int qF;
    private final int qG;
    private final String qH;
    private final int qu;
    private final int qv;
    private final int qw;
    private final int qx;
    private final int qy;
    private final int qz;

    public static final class Builder {
        private final a kB;
        private int qA;
        private int qB;
        private String qC;
        private int qD;
        private String qE;
        private int qF;
        private int qG;
        private String qH;
        private int qu;
        private int qv;
        private int qw;
        private int qx;
        private int qy;
        private int qz;

        public Builder() {
            this.kB = new a();
            this.qA = SearchAdRequest.ERROR_CODE_INTERNAL_ERROR;
        }

        public Builder addNetworkExtras(NetworkExtras networkExtras) {
            this.kB.a(networkExtras);
            return this;
        }

        public Builder addTestDevice(String deviceId) {
            this.kB.h(deviceId);
            return this;
        }

        public SearchAdRequest build() {
            return new SearchAdRequest();
        }

        public Builder setAnchorTextColor(int anchorTextColor) {
            this.qu = anchorTextColor;
            return this;
        }

        public Builder setBackgroundColor(int backgroundColor) {
            this.qv = backgroundColor;
            this.qw = Color.argb(SearchAdRequest.ERROR_CODE_INTERNAL_ERROR, SearchAdRequest.ERROR_CODE_INTERNAL_ERROR, SearchAdRequest.ERROR_CODE_INTERNAL_ERROR, SearchAdRequest.ERROR_CODE_INTERNAL_ERROR);
            this.qx = Color.argb(SearchAdRequest.ERROR_CODE_INTERNAL_ERROR, SearchAdRequest.ERROR_CODE_INTERNAL_ERROR, SearchAdRequest.ERROR_CODE_INTERNAL_ERROR, SearchAdRequest.ERROR_CODE_INTERNAL_ERROR);
            return this;
        }

        public Builder setBackgroundGradient(int top, int bottom) {
            this.qv = Color.argb(SearchAdRequest.ERROR_CODE_INTERNAL_ERROR, SearchAdRequest.ERROR_CODE_INTERNAL_ERROR, SearchAdRequest.ERROR_CODE_INTERNAL_ERROR, SearchAdRequest.ERROR_CODE_INTERNAL_ERROR);
            this.qw = bottom;
            this.qx = top;
            return this;
        }

        public Builder setBorderColor(int borderColor) {
            this.qy = borderColor;
            return this;
        }

        public Builder setBorderThickness(int borderThickness) {
            this.qz = borderThickness;
            return this;
        }

        public Builder setBorderType(int borderType) {
            this.qA = borderType;
            return this;
        }

        public Builder setCallButtonColor(int callButtonColor) {
            this.qB = callButtonColor;
            return this;
        }

        public Builder setCustomChannels(String channelIds) {
            this.qC = channelIds;
            return this;
        }

        public Builder setDescriptionTextColor(int descriptionTextColor) {
            this.qD = descriptionTextColor;
            return this;
        }

        public Builder setFontFace(String fontFace) {
            this.qE = fontFace;
            return this;
        }

        public Builder setHeaderTextColor(int headerTextColor) {
            this.qF = headerTextColor;
            return this;
        }

        public Builder setHeaderTextSize(int headerTextSize) {
            this.qG = headerTextSize;
            return this;
        }

        public Builder setLocation(Location location) {
            this.kB.a(location);
            return this;
        }

        public Builder setQuery(String query) {
            this.qH = query;
            return this;
        }

        public Builder tagForChildDirectedTreatment(boolean tagForChildDirectedTreatment) {
            this.kB.e(tagForChildDirectedTreatment);
            return this;
        }
    }

    static {
        DEVICE_ID_EMULATOR = aj.DEVICE_ID_EMULATOR;
    }

    private SearchAdRequest(Builder builder) {
        this.qu = builder.qu;
        this.qv = builder.qv;
        this.qw = builder.qw;
        this.qx = builder.qx;
        this.qy = builder.qy;
        this.qz = builder.qz;
        this.qA = builder.qA;
        this.qB = builder.qB;
        this.qC = builder.qC;
        this.qD = builder.qD;
        this.qE = builder.qE;
        this.qF = builder.qF;
        this.qG = builder.qG;
        this.qH = builder.qH;
        this.kA = new aj(builder.kB, this);
    }

    aj N() {
        return this.kA;
    }

    public int getAnchorTextColor() {
        return this.qu;
    }

    public int getBackgroundColor() {
        return this.qv;
    }

    public int getBackgroundGradientBottom() {
        return this.qw;
    }

    public int getBackgroundGradientTop() {
        return this.qx;
    }

    public int getBorderColor() {
        return this.qy;
    }

    public int getBorderThickness() {
        return this.qz;
    }

    public int getBorderType() {
        return this.qA;
    }

    public int getCallButtonColor() {
        return this.qB;
    }

    public String getCustomChannels() {
        return this.qC;
    }

    public int getDescriptionTextColor() {
        return this.qD;
    }

    public String getFontFace() {
        return this.qE;
    }

    public int getHeaderTextColor() {
        return this.qF;
    }

    public int getHeaderTextSize() {
        return this.qG;
    }

    public Location getLocation() {
        return this.kA.getLocation();
    }

    public <T extends NetworkExtras> T getNetworkExtras(Class<T> networkExtrasClass) {
        return this.kA.getNetworkExtras(networkExtrasClass);
    }

    public String getQuery() {
        return this.qH;
    }

    public boolean isTestDevice(Context context) {
        return this.kA.isTestDevice(context);
    }
}
