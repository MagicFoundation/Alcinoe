package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.identity.intents.model.UserAddress;

public final class FullWallet implements SafeParcelable {
    public static final Creator<FullWallet> CREATOR;
    String Yk;
    String Yl;
    ProxyCard Ym;
    String Yn;
    Address Yo;
    Address Yp;
    String[] Yq;
    UserAddress Yr;
    UserAddress Ys;
    InstrumentInfo[] Yt;
    private final int wj;

    static {
        CREATOR = new f();
    }

    private FullWallet() {
        this.wj = 1;
    }

    FullWallet(int versionCode, String googleTransactionId, String merchantTransactionId, ProxyCard proxyCard, String email, Address billingAddress, Address shippingAddress, String[] paymentDescriptions, UserAddress buyerBillingAddress, UserAddress buyerShippingAddress, InstrumentInfo[] instrumentInfos) {
        this.wj = versionCode;
        this.Yk = googleTransactionId;
        this.Yl = merchantTransactionId;
        this.Ym = proxyCard;
        this.Yn = email;
        this.Yo = billingAddress;
        this.Yp = shippingAddress;
        this.Yq = paymentDescriptions;
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

    public String getMerchantTransactionId() {
        return this.Yl;
    }

    public String[] getPaymentDescriptions() {
        return this.Yq;
    }

    public ProxyCard getProxyCard() {
        return this.Ym;
    }

    @Deprecated
    public Address getShippingAddress() {
        return this.Yp;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel out, int flags) {
        f.a(this, out, flags);
    }
}
