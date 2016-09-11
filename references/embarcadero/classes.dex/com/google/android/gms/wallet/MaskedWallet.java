package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.identity.intents.model.UserAddress;

public final class MaskedWallet implements SafeParcelable {
    public static final Creator<MaskedWallet> CREATOR;
    LoyaltyWalletObject[] YW;
    OfferWalletObject[] YX;
    String Yk;
    String Yl;
    String Yn;
    Address Yo;
    Address Yp;
    String[] Yq;
    UserAddress Yr;
    UserAddress Ys;
    InstrumentInfo[] Yt;
    private final int wj;

    static {
        CREATOR = new k();
    }

    private MaskedWallet() {
        this.wj = 2;
    }

    MaskedWallet(int versionCode, String googleTransactionId, String merchantTransactionId, String[] paymentDescriptions, String email, Address billingAddress, Address shippingAddress, LoyaltyWalletObject[] loyaltyWalletObjects, OfferWalletObject[] offerWalletObjects, UserAddress buyerBillingAddress, UserAddress buyerShippingAddress, InstrumentInfo[] instrumentInfos) {
        this.wj = versionCode;
        this.Yk = googleTransactionId;
        this.Yl = merchantTransactionId;
        this.Yq = paymentDescriptions;
        this.Yn = email;
        this.Yo = billingAddress;
        this.Yp = shippingAddress;
        this.YW = loyaltyWalletObjects;
        this.YX = offerWalletObjects;
        this.Yr = buyerBillingAddress;
        this.Ys = buyerShippingAddress;
        this.Yt = instrumentInfos;
    }

    public int describeContents() {
        return 0;
    }

    @Deprecated
    public Address getBillingAddress() {
        return this.Yo;
    }

    public UserAddress getBuyerBillingAddress() {
        return this.Yr;
    }

    public UserAddress getBuyerShippingAddress() {
        return this.Ys;
    }

    public String getEmail() {
        return this.Yn;
    }

    public String getGoogleTransactionId() {
        return this.Yk;
    }

    public InstrumentInfo[] getInstrumentInfos() {
        return this.Yt;
    }

    public LoyaltyWalletObject[] getLoyaltyWalletObjects() {
        return this.YW;
    }

    public String getMerchantTransactionId() {
        return this.Yl;
    }

    public OfferWalletObject[] getOfferWalletObjects() {
        return this.YX;
    }

    public String[] getPaymentDescriptions() {
        return this.Yq;
    }

    @Deprecated
    public Address getShippingAddress() {
        return this.Yp;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel dest, int flags) {
        k.a(this, dest, flags);
    }
}
