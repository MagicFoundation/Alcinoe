package com.google.analytics.tracking.android;

import android.text.TextUtils;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class Transaction {
    private final String mAffiliation;
    private final String mCurrencyCode;
    private final Map<String, Item> mItems;
    private final long mShippingCostInMicros;
    private final long mTotalCostInMicros;
    private final long mTotalTaxInMicros;
    private final String mTransactionId;

    public static final class Builder {
        private String mAffiliation;
        private String mCurrencyCode;
        private long mShippingCostInMicros;
        private final long mTotalCostInMicros;
        private long mTotalTaxInMicros;
        private final String mTransactionId;

        public Builder(String transactionId, long totalCostInMicros) {
            this.mAffiliation = null;
            this.mTotalTaxInMicros = 0;
            this.mShippingCostInMicros = 0;
            this.mCurrencyCode = null;
            if (TextUtils.isEmpty(transactionId)) {
                throw new IllegalArgumentException("orderId must not be empty or null");
            }
            this.mTransactionId = transactionId;
            this.mTotalCostInMicros = totalCostInMicros;
        }

        public Builder setAffiliation(String affiliation) {
            this.mAffiliation = affiliation;
            return this;
        }

        public Builder setTotalTaxInMicros(long totalTaxInMicros) {
            this.mTotalTaxInMicros = totalTaxInMicros;
            return this;
        }

        public Builder setShippingCostInMicros(long shippingCostInMicros) {
            this.mShippingCostInMicros = shippingCostInMicros;
            return this;
        }

        public Builder setCurrencyCode(String currencyCode) {
            this.mCurrencyCode = currencyCode;
            return this;
        }

        public Transaction build() {
            return new Transaction();
        }
    }

    public static final class Item {
        private final String mCategory;
        private final String mName;
        private final long mPriceInMicros;
        private final long mQuantity;
        private final String mSKU;

        public static final class Builder {
            private String mCategory;
            private final String mName;
            private final long mPriceInMicros;
            private final long mQuantity;
            private final String mSKU;

            public Builder(String SKU, String name, long priceInMicros, long quantity) {
                this.mCategory = null;
                if (TextUtils.isEmpty(SKU)) {
                    throw new IllegalArgumentException("SKU must not be empty or null");
                } else if (TextUtils.isEmpty(name)) {
                    throw new IllegalArgumentException("name must not be empty or null");
                } else {
                    this.mSKU = SKU;
                    this.mName = name;
                    this.mPriceInMicros = priceInMicros;
                    this.mQuantity = quantity;
                }
            }

            public Builder setProductCategory(String productCategory) {
                this.mCategory = productCategory;
                return this;
            }

            public Item build() {
                return new Item();
            }
        }

        private Item(Builder builder) {
            this.mSKU = builder.mSKU;
            this.mPriceInMicros = builder.mPriceInMicros;
            this.mQuantity = builder.mQuantity;
            this.mName = builder.mName;
            this.mCategory = builder.mCategory;
        }

        public String getSKU() {
            return this.mSKU;
        }

        public String getName() {
            return this.mName;
        }

        public String getCategory() {
            return this.mCategory;
        }

        public long getPriceInMicros() {
            return this.mPriceInMicros;
        }

        public long getQuantity() {
            return this.mQuantity;
        }
    }

    private Transaction(Builder builder) {
        this.mTransactionId = builder.mTransactionId;
        this.mTotalCostInMicros = builder.mTotalCostInMicros;
        this.mAffiliation = builder.mAffiliation;
        this.mTotalTaxInMicros = builder.mTotalTaxInMicros;
        this.mShippingCostInMicros = builder.mShippingCostInMicros;
        this.mCurrencyCode = builder.mCurrencyCode;
        this.mItems = new HashMap();
    }

    public String getTransactionId() {
        return this.mTransactionId;
    }

    public String getAffiliation() {
        return this.mAffiliation;
    }

    public long getTotalCostInMicros() {
        return this.mTotalCostInMicros;
    }

    public long getTotalTaxInMicros() {
        return this.mTotalTaxInMicros;
    }

    public long getShippingCostInMicros() {
        return this.mShippingCostInMicros;
    }

    public String getCurrencyCode() {
        return this.mCurrencyCode;
    }

    public void addItem(Item item) {
        this.mItems.put(item.getSKU(), item);
    }

    public List<Item> getItems() {
        return new ArrayList(this.mItems.values());
    }
}
