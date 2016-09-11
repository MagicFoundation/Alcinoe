package com.google.analytics.tracking.android;

import com.google.analytics.tracking.android.MetaModel.Formatter;
import com.google.android.gms.plus.PlusShare;
import java.text.DecimalFormat;

class MetaModelInitializer {
    private static final Formatter BOOLEAN_FORMATTER;
    private static final Formatter UP_TO_TWO_DIGIT_FLOAT_FORMATTER;

    static {
        BOOLEAN_FORMATTER = new Formatter() {
            public String format(String rawValue) {
                return Utils.safeParseBoolean(rawValue) ? "1" : "0";
            }
        };
        UP_TO_TWO_DIGIT_FLOAT_FORMATTER = new Formatter() {
            private final DecimalFormat mFloatFormat;

            {
                this.mFloatFormat = new DecimalFormat("0.##");
            }

            public String format(String rawValue) {
                return this.mFloatFormat.format(Utils.safeParseDouble(rawValue));
            }
        };
    }

    private MetaModelInitializer() {
    }

    public static void set(MetaModel m) {
        m.addField(ModelFields.API_VERSION, "v", null, null);
        m.addField("libraryVersion", "_v", null, null);
        m.addField(ModelFields.ANONYMIZE_IP, "aip", "0", BOOLEAN_FORMATTER);
        m.addField(ModelFields.TRACKING_ID, "tid", null, null);
        m.addField(ModelFields.HIT_TYPE, "t", null, null);
        m.addField(ModelFields.SESSION_CONTROL, "sc", null, null);
        m.addField("adSenseAdMobHitId", "a", null, null);
        m.addField("usage", "_u", null, null);
        m.addField(PlusShare.KEY_CONTENT_DEEP_LINK_METADATA_TITLE, "dt", null, null);
        m.addField(ModelFields.REFERRER, "dr", null, null);
        m.addField(ModelFields.LANGUAGE, "ul", null, null);
        m.addField(ModelFields.ENCODING, "de", null, null);
        m.addField(ModelFields.PAGE, "dp", null, null);
        m.addField(ModelFields.SCREEN_COLORS, "sd", null, null);
        m.addField(ModelFields.SCREEN_RESOLUTION, "sr", null, null);
        m.addField(ModelFields.VIEWPORT_SIZE, "vp", null, null);
        m.addField(ModelFields.JAVA_ENABLED, "je", "1", BOOLEAN_FORMATTER);
        m.addField(ModelFields.FLASH_VERSION, "fl", null, null);
        m.addField(ModelFields.CLIENT_ID, "cid", null, null);
        m.addField(ModelFields.CAMPAIGN_NAME, "cn", null, null);
        m.addField(ModelFields.CAMPAIGN_SOURCE, "cs", null, null);
        m.addField(ModelFields.CAMPAIGN_MEDIUM, "cm", null, null);
        m.addField(ModelFields.CAMPAIGN_KEYWORD, "ck", null, null);
        m.addField(ModelFields.CAMPAIGN_CONTENT, "cc", null, null);
        m.addField(ModelFields.CAMPAIGN_ID, "ci", null, null);
        m.addField(ModelFields.GCLID, ModelFields.GCLID, null, null);
        m.addField(ModelFields.DCLID, ModelFields.DCLID, null, null);
        m.addField(ModelFields.GMOB_T, ModelFields.GMOB_T, null, null);
        m.addField(ModelFields.EVENT_CATEGORY, "ec", null, null);
        m.addField(ModelFields.EVENT_ACTION, "ea", null, null);
        m.addField(ModelFields.EVENT_LABEL, "el", null, null);
        m.addField(ModelFields.EVENT_VALUE, "ev", null, null);
        m.addField(ModelFields.NON_INTERACTION, "ni", "0", BOOLEAN_FORMATTER);
        m.addField(ModelFields.SOCIAL_NETWORK, "sn", null, null);
        m.addField(ModelFields.SOCIAL_ACTION, "sa", null, null);
        m.addField(ModelFields.SOCIAL_TARGET, "st", null, null);
        m.addField(ModelFields.APP_NAME, "an", null, null);
        m.addField(ModelFields.APP_VERSION, "av", null, null);
        m.addField(PlusShare.KEY_CONTENT_DEEP_LINK_METADATA_DESCRIPTION, "cd", null, null);
        m.addField(ModelFields.APP_ID, "aid", null, null);
        m.addField(ModelFields.APP_INSTALLER_ID, "aiid", null, null);
        m.addField(ModelFields.TRANSACTION_ID, "ti", null, null);
        m.addField(ModelFields.TRANSACTION_AFFILIATION, "ta", null, null);
        m.addField(ModelFields.TRANSACTION_SHIPPING, "ts", null, null);
        m.addField(ModelFields.TRANSACTION_TOTAL, "tr", null, null);
        m.addField(ModelFields.TRANSACTION_TAX, "tt", null, null);
        m.addField("currencyCode", "cu", null, null);
        m.addField(ModelFields.ITEM_PRICE, "ip", null, null);
        m.addField(ModelFields.ITEM_CODE, "ic", null, null);
        m.addField(ModelFields.ITEM_NAME, "in", null, null);
        m.addField(ModelFields.ITEM_CATEGORY, "iv", null, null);
        m.addField(ModelFields.ITEM_QUANTITY, "iq", null, null);
        m.addField(ModelFields.EX_DESCRIPTION, "exd", null, null);
        m.addField(ModelFields.EX_FATAL, "exf", "1", BOOLEAN_FORMATTER);
        m.addField(ModelFields.TIMING_VAR, "utv", null, null);
        m.addField(ModelFields.TIMING_VALUE, "utt", null, null);
        m.addField(ModelFields.TIMING_CATEGORY, "utc", null, null);
        m.addField(ModelFields.TIMING_LABEL, "utl", null, null);
        m.addField(ModelFields.SAMPLE_RATE, "sf", "100", UP_TO_TWO_DIGIT_FLOAT_FORMATTER);
        m.addField("hitTime", "ht", null, null);
        m.addField(ModelFields.CUSTOM_DIMENSION, "cd", null, null);
        m.addField(ModelFields.CUSTOM_METRIC, "cm", null, null);
        m.addField(ModelFields.CONTENT_GROUPING, "cg", null, null);
    }
}
