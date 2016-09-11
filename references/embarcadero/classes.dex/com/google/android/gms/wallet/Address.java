package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

@Deprecated
public final class Address implements SafeParcelable {
    public static final Creator<Address> CREATOR;
    String KB;
    String KC;
    String KD;
    String KI;
    String KK;
    boolean KL;
    String KM;
    String Yd;
    String Ye;
    String name;
    String oQ;
    private final int wj;

    static {
        CREATOR = new a();
    }

    Address() {
        this.wj = 1;
    }

    Address(int versionCode, String name, String address1, String address2, String address3, String countryCode, String city, String state, String postalCode, String phoneNumber, boolean isPostBox, String companyName) {
        this.wj = versionCode;
        this.name = name;
        this.KB = address1;
        this.KC = address2;
        this.KD = address3;
        this.oQ = countryCode;
        this.Yd = city;
        this.Ye = state;
        this.KI = postalCode;
        this.KK = phoneNumber;
        this.KL = isPostBox;
        this.KM = companyName;
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

    public String getCity() {
        return this.Yd;
    }

    public String getCompanyName() {
        return this.KM;
    }

    public String getCountryCode() {
        return this.oQ;
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

    public String getState() {
        return this.Ye;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public boolean isPostBox() {
        return this.KL;
    }

    public void writeToParcel(Parcel out, int flags) {
        a.a(this, out, flags);
    }
}
