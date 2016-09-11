package com.google.android.gms.wallet;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.identity.intents.model.CountrySpecification;
import java.util.ArrayList;
import java.util.Collection;

public final class MaskedWalletRequest implements SafeParcelable {
    public static final Creator<MaskedWalletRequest> CREATOR;
    boolean YY;
    boolean YZ;
    String Yg;
    String Yl;
    Cart Yu;
    boolean Za;
    String Zb;
    String Zc;
    boolean Zd;
    boolean Ze;
    CountrySpecification[] Zf;
    boolean Zg;
    boolean Zh;
    ArrayList<CountrySpecification> Zi;
    private final int wj;

    public final class Builder {
        final /* synthetic */ MaskedWalletRequest Zj;

        private Builder(MaskedWalletRequest maskedWalletRequest) {
            this.Zj = maskedWalletRequest;
        }

        public Builder addAllowedCountrySpecificationForShipping(CountrySpecification countrySpecification) {
            if (this.Zj.Zi == null) {
                this.Zj.Zi = new ArrayList();
            }
            this.Zj.Zi.add(countrySpecification);
            return this;
        }

        public Builder addAllowedCountrySpecificationsForShipping(Collection<CountrySpecification> countrySpecifications) {
            if (countrySpecifications != null) {
                if (this.Zj.Zi == null) {
                    this.Zj.Zi = new ArrayList();
                }
                this.Zj.Zi.addAll(countrySpecifications);
            }
            return this;
        }

        public MaskedWalletRequest build() {
            return this.Zj;
        }

        public Builder setAllowDebitCard(boolean allowDebitCard) {
            this.Zj.Zh = allowDebitCard;
            return this;
        }

        public Builder setAllowPrepaidCard(boolean allowPrepaidCard) {
            this.Zj.Zg = allowPrepaidCard;
            return this;
        }

        public Builder setCart(Cart cart) {
            this.Zj.Yu = cart;
            return this;
        }

        public Builder setCurrencyCode(String currencyCode) {
            this.Zj.Yg = currencyCode;
            return this;
        }

        public Builder setEstimatedTotalPrice(String estimatedTotalPrice) {
            this.Zj.Zb = estimatedTotalPrice;
            return this;
        }

        public Builder setIsBillingAgreement(boolean isBillingAgreement) {
            this.Zj.Ze = isBillingAgreement;
            return this;
        }

        public Builder setMerchantName(String merchantName) {
            this.Zj.Zc = merchantName;
            return this;
        }

        public Builder setMerchantTransactionId(String merchantTransactionId) {
            this.Zj.Yl = merchantTransactionId;
            return this;
        }

        public Builder setPhoneNumberRequired(boolean phoneNumberRequired) {
            this.Zj.YY = phoneNumberRequired;
            return this;
        }

        public Builder setShippingAddressRequired(boolean shippingAddressRequired) {
            this.Zj.YZ = shippingAddressRequired;
            return this;
        }

        public Builder setShouldRetrieveWalletObjects(boolean shouldRetrieveWalletObjects) {
            this.Zj.Zd = shouldRetrieveWalletObjects;
            return this;
        }

        public Builder setUseMinimalBillingAddress(boolean useMinimalBillingAddress) {
            this.Zj.Za = useMinimalBillingAddress;
            return this;
        }
    }

    static {
        CREATOR = new l();
    }

    MaskedWalletRequest() {
        this.wj = 3;
        this.Zg = true;
        this.Zh = true;
    }

    MaskedWalletRequest(int versionCode, String merchantTransactionId, boolean phoneNumberRequired, boolean shippingAddressRequired, boolean useMinimalBillingAddress, String estimatedTotalPrice, String currencyCode, String merchantName, Cart cart, boolean shouldRetrieveWalletObjects, boolean isBillingAgreement, CountrySpecification[] allowedShippingCountrySpecifications, boolean allowPrepaidCard, boolean allowDebitCard, ArrayList<CountrySpecification> allowedCountrySpecificationsForShipping) {
        this.wj = versionCode;
        this.Yl = merchantTransactionId;
        this.YY = phoneNumberRequired;
        this.YZ = shippingAddressRequired;
        this.Za = useMinimalBillingAddress;
        this.Zb = estimatedTotalPrice;
        this.Yg = currencyCode;
        this.Zc = merchantName;
        this.Yu = cart;
        this.Zd = shouldRetrieveWalletObjects;
        this.Ze = isBillingAgreement;
        this.Zf = allowedShippingCountrySpecifications;
        this.Zg = allowPrepaidCard;
        this.Zh = allowDebitCard;
        this.Zi = allowedCountrySpecificationsForShipping;
    }

    public static Builder newBuilder() {
        MaskedWalletRequest maskedWalletRequest = new MaskedWalletRequest();
        maskedWalletRequest.getClass();
        return new Builder(null);
    }

    public boolean allowDebitCard() {
        return this.Zh;
    }

    public boolean allowPrepaidCard() {
        return this.Zg;
    }

    public int describeContents() {
        return 0;
    }

    public ArrayList<CountrySpecification> getAllowedCountrySpecificationsForShipping() {
        return this.Zi;
    }

    public CountrySpecification[] getAllowedShippingCountrySpecifications() {
        return this.Zf;
    }

    public Cart getCart() {
        return this.Yu;
    }

    public String getCurrencyCode() {
        return this.Yg;
    }

    public String getEstimatedTotalPrice() {
        return this.Zb;
    }

    public String getMerchantName() {
        return this.Zc;
    }

    public String getMerchantTransactionId() {
        return this.Yl;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public boolean isBillingAgreement() {
        return this.Ze;
    }

    public boolean isPhoneNumberRequired() {
        return this.YY;
    }

    public boolean isShippingAddressRequired() {
        return this.YZ;
    }

    public boolean shouldRetrieveWalletObjects() {
        return this.Zd;
    }

    public boolean useMinimalBillingAddress() {
        return this.Za;
    }

    public void writeToParcel(Parcel dest, int flags) {
        l.a(this, dest, flags);
    }
}
