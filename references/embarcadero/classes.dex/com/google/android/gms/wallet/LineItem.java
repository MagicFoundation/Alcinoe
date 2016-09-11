package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;

public final class LineItem implements SafeParcelable {
    public static final Creator<LineItem> CREATOR;
    int YA;
    String Yf;
    String Yg;
    String Yy;
    String Yz;
    String description;
    private final int wj;

    public final class Builder {
        final /* synthetic */ LineItem YB;

        private Builder(LineItem lineItem) {
            this.YB = lineItem;
        }

        public LineItem build() {
            return this.YB;
        }

        public Builder setCurrencyCode(String currencyCode) {
            this.YB.Yg = currencyCode;
            return this;
        }

        public Builder setDescription(String description) {
            this.YB.description = description;
            return this;
        }

        public Builder setQuantity(String quantity) {
            this.YB.Yy = quantity;
            return this;
        }

        public Builder setRole(int role) {
            this.YB.YA = role;
            return this;
        }

        public Builder setTotalPrice(String totalPrice) {
            this.YB.Yf = totalPrice;
            return this;
        }

        public Builder setUnitPrice(String unitPrice) {
            this.YB.Yz = unitPrice;
            return this;
        }
    }

    public interface Role {
        public static final int REGULAR = 0;
        public static final int SHIPPING = 2;
        public static final int TAX = 1;
    }

    static {
        CREATOR = new i();
    }

    LineItem() {
        this.wj = 1;
        this.YA = 0;
    }

    LineItem(int versionCode, String description, String quantity, String unitPrice, String totalPrice, int role, String currencyCode) {
        this.wj = versionCode;
        this.description = description;
        this.Yy = quantity;
        this.Yz = unitPrice;
        this.Yf = totalPrice;
        this.YA = role;
        this.Yg = currencyCode;
    }

    public static Builder newBuilder() {
        LineItem lineItem = new LineItem();
        lineItem.getClass();
        return new Builder(null);
    }

    public int describeContents() {
        return 0;
    }

    public String getCurrencyCode() {
        return this.Yg;
    }

    public String getDescription() {
        return this.description;
    }

    public String getQuantity() {
        return this.Yy;
    }

    public int getRole() {
        return this.YA;
    }

    public String getTotalPrice() {
        return this.Yf;
    }

    public String getUnitPrice() {
        return this.Yz;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel dest, int flags) {
        i.a(this, dest, flags);
    }
}
