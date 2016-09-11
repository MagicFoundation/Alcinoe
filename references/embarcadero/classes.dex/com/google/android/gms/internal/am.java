package com.google.android.gms.internal;

import android.os.Parcel;
import com.google.android.gms.ads.search.SearchAdRequest;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public final class am implements SafeParcelable {
    public static final an CREATOR;
    public final int backgroundColor;
    public final int lI;
    public final int lJ;
    public final int lK;
    public final int lL;
    public final int lM;
    public final int lN;
    public final int lO;
    public final String lP;
    public final int lQ;
    public final String lR;
    public final int lS;
    public final int lT;
    public final String lU;
    public final int versionCode;

    static {
        CREATOR = new an();
    }

    am(int i, int i2, int i3, int i4, int i5, int i6, int i7, int i8, int i9, String str, int i10, String str2, int i11, int i12, String str3) {
        this.versionCode = i;
        this.lI = i2;
        this.backgroundColor = i3;
        this.lJ = i4;
        this.lK = i5;
        this.lL = i6;
        this.lM = i7;
        this.lN = i8;
        this.lO = i9;
        this.lP = str;
        this.lQ = i10;
        this.lR = str2;
        this.lS = i11;
        this.lT = i12;
        this.lU = str3;
    }

    public am(SearchAdRequest searchAdRequest) {
        this.versionCode = 1;
        this.lI = searchAdRequest.getAnchorTextColor();
        this.backgroundColor = searchAdRequest.getBackgroundColor();
        this.lJ = searchAdRequest.getBackgroundGradientBottom();
        this.lK = searchAdRequest.getBackgroundGradientTop();
        this.lL = searchAdRequest.getBorderColor();
        this.lM = searchAdRequest.getBorderThickness();
        this.lN = searchAdRequest.getBorderType();
        this.lO = searchAdRequest.getCallButtonColor();
        this.lP = searchAdRequest.getCustomChannels();
        this.lQ = searchAdRequest.getDescriptionTextColor();
        this.lR = searchAdRequest.getFontFace();
        this.lS = searchAdRequest.getHeaderTextColor();
        this.lT = searchAdRequest.getHeaderTextSize();
        this.lU = searchAdRequest.getQuery();
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel out, int flags) {
        an.a(this, out, flags);
    }
}
