package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.internal.fj;
import com.google.android.gms.internal.jj;
import com.google.android.gms.internal.jl;
import com.google.android.gms.internal.jp;
import com.google.android.gms.internal.jr;
import com.google.android.gms.internal.jt;
import com.google.android.gms.internal.jv;
import com.google.android.gms.maps.model.LatLng;
import java.util.ArrayList;

public final class LoyaltyWalletObject implements SafeParcelable {
    public static final Creator<LoyaltyWalletObject> CREATOR;
    String YC;
    String YD;
    String YE;
    String YF;
    String YG;
    String YH;
    String YI;
    String YJ;
    String YK;
    ArrayList<jv> YL;
    jr YM;
    ArrayList<LatLng> YN;
    String YO;
    String YP;
    ArrayList<jj> YQ;
    boolean YR;
    ArrayList<jt> YS;
    ArrayList<jp> YT;
    ArrayList<jt> YU;
    jl YV;
    String eN;
    int state;
    private final int wj;

    static {
        CREATOR = new j();
    }

    LoyaltyWalletObject() {
        this.wj = 4;
        this.YL = fj.eH();
        this.YN = fj.eH();
        this.YQ = fj.eH();
        this.YS = fj.eH();
        this.YT = fj.eH();
        this.YU = fj.eH();
    }

    LoyaltyWalletObject(int versionCode, String id, String accountId, String issuerName, String programName, String accountName, String barcodeAlternateText, String barcodeType, String barcodeValue, String barcodeLabel, String classId, int state, ArrayList<jv> messages, jr validTimeInterval, ArrayList<LatLng> locations, String infoModuleDataHexFontColor, String infoModuleDataHexBackgroundColor, ArrayList<jj> infoModuleDataLabelValueRows, boolean infoModuleDataShowLastUpdateTime, ArrayList<jt> imageModuleDataMainImageUris, ArrayList<jp> textModulesData, ArrayList<jt> linksModuleDataUris, jl loyaltyPoints) {
        this.wj = versionCode;
        this.eN = id;
        this.YC = accountId;
        this.YD = issuerName;
        this.YE = programName;
        this.YF = accountName;
        this.YG = barcodeAlternateText;
        this.YH = barcodeType;
        this.YI = barcodeValue;
        this.YJ = barcodeLabel;
        this.YK = classId;
        this.state = state;
        this.YL = messages;
        this.YM = validTimeInterval;
        this.YN = locations;
        this.YO = infoModuleDataHexFontColor;
        this.YP = infoModuleDataHexBackgroundColor;
        this.YQ = infoModuleDataLabelValueRows;
        this.YR = infoModuleDataShowLastUpdateTime;
        this.YS = imageModuleDataMainImageUris;
        this.YT = textModulesData;
        this.YU = linksModuleDataUris;
        this.YV = loyaltyPoints;
    }

    public int describeContents() {
        return 0;
    }

    public String getAccountId() {
        return this.YC;
    }

    public String getAccountName() {
        return this.YF;
    }

    public String getBarcodeAlternateText() {
        return this.YG;
    }

    public String getBarcodeType() {
        return this.YH;
    }

    public String getBarcodeValue() {
        return this.YI;
    }

    public String getId() {
        return this.eN;
    }

    public String getIssuerName() {
        return this.YD;
    }

    public String getProgramName() {
        return this.YE;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel dest, int flags) {
        j.a(this, dest, flags);
    }
}
