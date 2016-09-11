package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import java.util.ArrayList;
import java.util.List;

public final class Cart implements SafeParcelable {
    public static final Creator<Cart> CREATOR;
    String Yf;
    String Yg;
    ArrayList<LineItem> Yh;
    private final int wj;

    public final class Builder {
        final /* synthetic */ Cart Yi;

        private Builder(Cart cart) {
            this.Yi = cart;
        }

        public Builder addLineItem(LineItem lineItem) {
            this.Yi.Yh.add(lineItem);
            return this;
        }

        public Cart build() {
            return this.Yi;
        }

        public Builder setCurrencyCode(String currencyCode) {
            this.Yi.Yg = currencyCode;
            return this;
        }

        public Builder setLineItems(List<LineItem> lineItems) {
            this.Yi.Yh.clear();
            this.Yi.Yh.addAll(lineItems);
            return this;
        }

        public Builder setTotalPrice(String totalPrice) {
            this.Yi.Yf = totalPrice;
            return this;
        }
    }

    static {
        CREATOR = new b();
    }

    Cart() {
        this.wj = 1;
        this.Yh = new ArrayList();
    }

    Cart(int versionCode, String totalPrice, String currencyCode, ArrayList<LineItem> lineItems) {
        this.wj = versionCode;
        this.Yf = totalPrice;
        this.Yg = currencyCode;
        this.Yh = lineItems;
    }

    public static Builder newBuilder() {
        Cart cart = new Cart();
        cart.getClass();
        return new Builder(null);
    }

    public int describeContents() {
        return 0;
    }

    public String getCurrencyCode() {
        return this.Yg;
    }

    public ArrayList<LineItem> getLineItems() {
        return this.Yh;
    }

    public String getTotalPrice() {
        return this.Yf;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel dest, int flags) {
        b.a(this, dest, flags);
    }
}
