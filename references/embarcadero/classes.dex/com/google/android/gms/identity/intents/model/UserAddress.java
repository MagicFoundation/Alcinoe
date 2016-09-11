package com.google.android.gms.identity.intents.model;

import android.content.Intent;
import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.identity.intents.AddressConstants.Extras;

public final class UserAddress implements SafeParcelable {
    public static final Creator<UserAddress> CREATOR;
    String KB;
    String KC;
    String KD;
    String KE;
    String KF;
    String KG;
    String KH;
    String KI;
    String KJ;
    String KK;
    boolean KL;
    String KM;
    String KN;
    String name;
    String oQ;
    private final int wj;

    static {
        CREATOR = new b();
    }

    UserAddress() {
        this.wj = 1;
    }

    UserAddress(int versionCode, String name, String address1, String address2, String address3, String address4, String address5, String administrativeArea, String locality, String countryCode, String postalCode, String sortingCode, String phoneNumber, boolean isPostBox, String companyName, String emailAddress) {
        this.wj = versionCode;
        this.name = name;
        this.KB = address1;
        this.KC = address2;
        this.KD = address3;
        this.KE = address4;
        this.KF = address5;
        this.KG = administrativeArea;
        this.KH = locality;
        this.oQ = countryCode;
        this.KI = postalCode;
        this.KJ = sortingCode;
        this.KK = phoneNumber;
        this.KL = isPostBox;
        this.KM = companyName;
        this.KN = emailAddress;
    }

    public static UserAddress fromIntent(Intent data) {
        return (data == null || !data.hasExtra(Extras.EXTRA_ADDRESS)) ? null : (UserAddress) data.getParcelableExtra(Extras.EXTRA_ADDRESS);
    }

    public int describeContents() {
        return 0;
    }

    public String getAddress1() {
        return this.KB;
    }

    public String getAddress2() {
        return this.KC;
    }

    public String getAddress3() {
        return this.KD;
    }

    public String getAddress4() {
        return this.KE;
    }

    public String getAddress5() {
        return this.KF;
    }

    public String getAdministrativeArea() {
        return this.KG;
    }

    public String getCompanyName() {
        return this.KM;
    }

    public String getCountryCode() {
        return this.oQ;
    }

    public String getEmailAddress() {
        return this.KN;
    }

    public String getLocality() {
        return this.KH;
    }

    public String getName() {
        return this.name;
    }

    public String getPhoneNumber() {
        return this.KK;
    }

    public String getPostalCode() {
        return this.KI;
    }

    public String getSortingCode() {
        return this.KJ;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public boolean isPostBox() {
        return this.KL;
    }

    public void writeToParcel(Parcel out, int flags) {
        b.a(this, out, flags);
    }
}
