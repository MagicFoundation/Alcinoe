package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public final class FullWalletRequest implements SafeParcelable {
    public static final Creator<FullWalletRequest> CREATOR;
    String Yk;
    String Yl;
    Cart Yu;
    private final int wj;

    public final class Builder {
        final /* synthetic */ FullWalletRequest Yv;

        private Builder(FullWalletRequest fullWalletRequest) {
            this.Yv = fullWalletRequest;
        }

        public FullWalletRequest build() {
            return this.Yv;
        }

        public Builder setCart(Cart cart) {
            this.Yv.Yu = cart;
            return this;
        }

        public Builder setGoogleTransactionId(String googleTransactionId) {
            this.Yv.Yk = googleTransactionId;
            return this;
        }

        public Builder setMerchantTransactionId(String merchantTransactionId) {
            this.Yv.Yl = merchantTransactionId;
            return this;
        }
    }

    static {
        CREATOR = new g();
    }

    FullWalletRequest() {
        this.wj = 1;
    }

    FullWalletRequest(int versionCode, String googleTransactionId, String merchantTransactionId, Cart cart) {
        this.wj = versionCode;
        this.Yk = googleTransactionId;
        this.Yl = merchantTransactionId;
        this.Yu = cart;
    }

    public static Builder newBuilder() {
        FullWalletRequest fullWalletRequest = new FullWalletRequest();
        fullWalletRequest.getClass();
        return new Builder(null);
    }

    public int describeContents() {
        return 0;
    }

    public Cart getCart() {
        return this.Yu;
    }

    public String getGoogleTransactionId() {
        return this.Yk;
    }

    public String getMerchantTransactionId() {
        return this.Yl;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel dest, int flags) {
        g.a(this, dest, flags);
    }
}
