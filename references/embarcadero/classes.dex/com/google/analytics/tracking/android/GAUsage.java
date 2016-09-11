package com.google.analytics.tracking.android;

import com.google.android.gms.common.util.VisibleForTesting;
import java.util.SortedSet;
import java.util.TreeSet;

class GAUsage {
    private static final String BASE_64_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
    private static final GAUsage INSTANCE;
    private boolean mDisableUsage;
    private StringBuilder mSequence;
    private SortedSet<Field> mUsedFields;

    public enum Field {
        TRACK_VIEW,
        TRACK_VIEW_WITH_APPSCREEN,
        TRACK_EVENT,
        TRACK_TRANSACTION,
        TRACK_EXCEPTION_WITH_DESCRIPTION,
        TRACK_EXCEPTION_WITH_THROWABLE,
        BLANK_06,
        TRACK_TIMING,
        TRACK_SOCIAL,
        GET,
        SET,
        SEND,
        SET_START_SESSION,
        BLANK_13,
        SET_APP_NAME,
        BLANK_15,
        SET_APP_VERSION,
        BLANK_17,
        SET_APP_SCREEN,
        GET_TRACKING_ID,
        SET_ANONYMIZE_IP,
        GET_ANONYMIZE_IP,
        SET_SAMPLE_RATE,
        GET_SAMPLE_RATE,
        SET_USE_SECURE,
        GET_USE_SECURE,
        SET_REFERRER,
        SET_CAMPAIGN,
        SET_APP_ID,
        GET_APP_ID,
        SET_EXCEPTION_PARSER,
        GET_EXCEPTION_PARSER,
        CONSTRUCT_TRANSACTION,
        CONSTRUCT_EXCEPTION,
        CONSTRUCT_RAW_EXCEPTION,
        CONSTRUCT_TIMING,
        CONSTRUCT_SOCIAL,
        SET_DEBUG,
        GET_DEBUG,
        GET_TRACKER,
        GET_DEFAULT_TRACKER,
        SET_DEFAULT_TRACKER,
        SET_APP_OPT_OUT,
        REQUEST_APP_OPT_OUT,
        DISPATCH,
        SET_DISPATCH_PERIOD,
        BLANK_48,
        REPORT_UNCAUGHT_EXCEPTIONS,
        SET_AUTO_ACTIVITY_TRACKING,
        SET_SESSION_TIMEOUT,
        CONSTRUCT_EVENT,
        CONSTRUCT_ITEM,
        SET_APP_INSTALLER_ID,
        GET_APP_INSTALLER_ID
    }

    static {
        INSTANCE = new GAUsage();
    }

    public static GAUsage getInstance() {
        return INSTANCE;
    }

    @VisibleForTesting
    static GAUsage getPrivateInstance() {
        return new GAUsage();
    }

    private GAUsage() {
        this.mUsedFields = new TreeSet();
        this.mSequence = new StringBuilder();
        this.mDisableUsage = false;
    }

    public synchronized void setDisableUsage(boolean disableUsage) {
        this.mDisableUsage = disableUsage;
    }

    public synchronized void setUsage(Field field) {
        if (!this.mDisableUsage) {
            this.mUsedFields.add(field);
            this.mSequence.append(BASE_64_CHARS.charAt(field.ordinal()));
        }
    }

    public synchronized String getAndClearUsage() {
        StringBuilder result;
        result = new StringBuilder();
        int spot = 0;
        int nextBoundary = 6;
        while (this.mUsedFields.size() > 0) {
            Field f = (Field) this.mUsedFields.first();
            this.mUsedFields.remove(f);
            int nextLoc = f.ordinal();
            while (nextLoc >= nextBoundary) {
                result.append(BASE_64_CHARS.charAt(spot));
                spot = 0;
                nextBoundary += 6;
            }
            spot += 1 << (f.ordinal() % 6);
        }
        if (spot > 0 || result.length() == 0) {
            result.append(BASE_64_CHARS.charAt(spot));
        }
        this.mUsedFields.clear();
        return result.toString();
    }

    public synchronized String getAndClearSequence() {
        String result;
        if (this.mSequence.length() > 0) {
            this.mSequence.insert(0, ".");
        }
        result = this.mSequence.toString();
        this.mSequence = new StringBuilder();
        return result;
    }
}
