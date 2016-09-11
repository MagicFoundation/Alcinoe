package com.google.android.gms.analytics;

import android.text.TextUtils;
import com.google.analytics.tracking.android.ModelFields;
import com.google.android.gms.analytics.u.a;
import com.google.android.gms.tagmanager.DataLayer;
import java.util.HashMap;
import java.util.Map;

public class HitBuilders {

    protected static class HitBuilder<T extends HitBuilder> {
        private Map<String, String> tO;

        protected HitBuilder() {
            this.tO = new HashMap();
        }

        public Map<String, String> build() {
            return this.tO;
        }

        protected String get(String paramName) {
            return (String) this.tO.get(paramName);
        }

        public final T set(String paramName, String paramValue) {
            u.bR().a(a.MAP_BUILDER_SET);
            if (paramName != null) {
                this.tO.put(paramName, paramValue);
            } else {
                aa.w(" HitBuilder.set() called with a null paramName.");
            }
            return this;
        }

        public final T setAll(Map<String, String> params) {
            u.bR().a(a.MAP_BUILDER_SET_ALL);
            if (params != null) {
                this.tO.putAll(new HashMap(params));
            }
            return this;
        }

        public T setCampaignParamsFromUrl(String utmParams) {
            u.bR().a(a.MAP_BUILDER_SET_CAMPAIGN_PARAMS);
            Object J = ak.J(utmParams);
            if (!TextUtils.isEmpty(J)) {
                Map I = ak.I(J);
                set("&cc", (String) I.get("utm_content"));
                set("&cm", (String) I.get("utm_medium"));
                set("&cn", (String) I.get("utm_campaign"));
                set("&cs", (String) I.get("utm_source"));
                set("&ck", (String) I.get("utm_term"));
                set("&ci", (String) I.get("utm_id"));
                set("&gclid", (String) I.get(ModelFields.GCLID));
                set("&dclid", (String) I.get(ModelFields.DCLID));
                set("&gmob_t", (String) I.get(ModelFields.GMOB_T));
            }
            return this;
        }

        public T setCustomDimension(int index, String dimension) {
            set(o.q(index), dimension);
            return this;
        }

        public T setCustomMetric(int index, float metric) {
            set(o.r(index), Float.toString(metric));
            return this;
        }

        protected T setHitType(String hitType) {
            set("&t", hitType);
            return this;
        }

        public T setNewSession() {
            set("&sc", "start");
            return this;
        }

        public T setNonInteraction(boolean nonInteraction) {
            set("&ni", ak.s(nonInteraction));
            return this;
        }
    }

    public static class AppViewBuilder extends HitBuilder<AppViewBuilder> {
        public AppViewBuilder() {
            u.bR().a(a.CONSTRUCT_APP_VIEW);
            set("&t", ModelFields.APP_VIEW);
        }

        public /* bridge */ /* synthetic */ Map build() {
            return super.build();
        }
    }

    public static class EventBuilder extends HitBuilder<EventBuilder> {
        public EventBuilder() {
            u.bR().a(a.CONSTRUCT_EVENT);
            set("&t", DataLayer.EVENT_KEY);
        }

        public EventBuilder(String category, String action) {
            this();
            setCategory(category);
            setAction(action);
        }

        public /* bridge */ /* synthetic */ Map build() {
            return super.build();
        }

        public EventBuilder setAction(String action) {
            set("&ea", action);
            return this;
        }

        public EventBuilder setCategory(String category) {
            set("&ec", category);
            return this;
        }

        public EventBuilder setLabel(String label) {
            set("&el", label);
            return this;
        }

        public EventBuilder setValue(long value) {
            set("&ev", Long.toString(value));
            return this;
        }
    }

    public static class ExceptionBuilder extends HitBuilder<ExceptionBuilder> {
        public ExceptionBuilder() {
            u.bR().a(a.CONSTRUCT_EXCEPTION);
            set("&t", ModelFields.EXCEPTION);
        }

        public /* bridge */ /* synthetic */ Map build() {
            return super.build();
        }

        public ExceptionBuilder setDescription(String description) {
            set("&exd", description);
            return this;
        }

        public ExceptionBuilder setFatal(boolean fatal) {
            set("&exf", ak.s(fatal));
            return this;
        }
    }

    public static class ItemBuilder extends HitBuilder<ItemBuilder> {
        public ItemBuilder() {
            u.bR().a(a.CONSTRUCT_ITEM);
            set("&t", ModelFields.ITEM);
        }

        public /* bridge */ /* synthetic */ Map build() {
            return super.build();
        }

        public ItemBuilder setCategory(String category) {
            set("&iv", category);
            return this;
        }

        public ItemBuilder setCurrencyCode(String currencyCode) {
            set("&cu", currencyCode);
            return this;
        }

        public ItemBuilder setName(String name) {
            set("&in", name);
            return this;
        }

        public ItemBuilder setPrice(double price) {
            set("&ip", Double.toString(price));
            return this;
        }

        public ItemBuilder setQuantity(long quantity) {
            set("&iq", Long.toString(quantity));
            return this;
        }

        public ItemBuilder setSku(String sku) {
            set("&ic", sku);
            return this;
        }

        public ItemBuilder setTransactionId(String transactionid) {
            set("&ti", transactionid);
            return this;
        }
    }

    public static class SocialBuilder extends HitBuilder<SocialBuilder> {
        public SocialBuilder() {
            u.bR().a(a.CONSTRUCT_SOCIAL);
            set("&t", ModelFields.SOCIAL);
        }

        public /* bridge */ /* synthetic */ Map build() {
            return super.build();
        }

        public SocialBuilder setAction(String action) {
            set("&sa", action);
            return this;
        }

        public SocialBuilder setNetwork(String network) {
            set("&sn", network);
            return this;
        }

        public SocialBuilder setTarget(String target) {
            set("&st", target);
            return this;
        }
    }

    public static class TimingBuilder extends HitBuilder<TimingBuilder> {
        public TimingBuilder() {
            u.bR().a(a.CONSTRUCT_TIMING);
            set("&t", ModelFields.TIMING);
        }

        public TimingBuilder(String category, String variable, long value) {
            this();
            setVariable(variable);
            setValue(value);
            setCategory(category);
        }

        public /* bridge */ /* synthetic */ Map build() {
            return super.build();
        }

        public TimingBuilder setCategory(String category) {
            set("&utc", category);
            return this;
        }

        public TimingBuilder setLabel(String label) {
            set("&utl", label);
            return this;
        }

        public TimingBuilder setValue(long value) {
            set("&utt", Long.toString(value));
            return this;
        }

        public TimingBuilder setVariable(String variable) {
            set("&utv", variable);
            return this;
        }
    }

    public static class TransactionBuilder extends HitBuilder<TransactionBuilder> {
        public TransactionBuilder() {
            u.bR().a(a.CONSTRUCT_TRANSACTION);
            set("&t", "transaction");
        }

        public /* bridge */ /* synthetic */ Map build() {
            return super.build();
        }

        public TransactionBuilder setAffiliation(String affiliation) {
            set("&ta", affiliation);
            return this;
        }

        public TransactionBuilder setCurrencyCode(String currencyCode) {
            set("&cu", currencyCode);
            return this;
        }

        public TransactionBuilder setRevenue(double revenue) {
            set("&tr", Double.toString(revenue));
            return this;
        }

        public TransactionBuilder setShipping(double shipping) {
            set("&ts", Double.toString(shipping));
            return this;
        }

        public TransactionBuilder setTax(double tax) {
            set("&tt", Double.toString(tax));
            return this;
        }

        public TransactionBuilder setTransactionId(String transactionid) {
            set("&ti", transactionid);
            return this;
        }
    }
}
