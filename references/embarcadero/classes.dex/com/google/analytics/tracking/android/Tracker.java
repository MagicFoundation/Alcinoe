package com.google.analytics.tracking.android;

import android.text.TextUtils;
import com.google.analytics.tracking.android.GAUsage.Field;
import com.google.analytics.tracking.android.Transaction.Item;
import com.google.android.gms.common.util.VisibleForTesting;
import com.google.android.gms.plus.PlusShare;
import com.google.android.gms.tagmanager.DataLayer;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

public class Tracker {
    private static final DecimalFormat DF;
    static final long MAX_TOKENS = 120000;
    static final long TIME_PER_TOKEN_MILLIS = 2000;
    private volatile ExceptionParser mExceptionParser;
    private final TrackerHandler mHandler;
    private boolean mIsThrottlingEnabled;
    private volatile boolean mIsTrackerClosed;
    private volatile boolean mIsTrackingStarted;
    private long mLastTrackTime;
    private final SimpleModel mModel;
    private long mTokens;

    private static class SimpleModel {
        private Map<String, String> permanentMap;
        private Map<String, String> temporaryMap;

        private SimpleModel() {
            this.temporaryMap = new HashMap();
            this.permanentMap = new HashMap();
        }

        public synchronized void setForNextHit(String key, String value) {
            this.temporaryMap.put(key, value);
        }

        public synchronized void set(String key, String value) {
            this.permanentMap.put(key, value);
        }

        public synchronized void clearTemporaryValues() {
            this.temporaryMap.clear();
        }

        public synchronized String get(String key) {
            String result;
            result = (String) this.temporaryMap.get(key);
            if (result == null) {
                result = (String) this.permanentMap.get(key);
            }
            return result;
        }

        public synchronized void setAll(Map<String, String> keysAndValues, Boolean isForNextHit) {
            if (isForNextHit.booleanValue()) {
                this.temporaryMap.putAll(keysAndValues);
            } else {
                this.permanentMap.putAll(keysAndValues);
            }
        }

        public synchronized Map<String, String> getKeysAndValues() {
            Map<String, String> result;
            result = new HashMap(this.permanentMap);
            result.putAll(this.temporaryMap);
            return result;
        }
    }

    static {
        DF = new DecimalFormat("0.######", new DecimalFormatSymbols(Locale.US));
    }

    Tracker() {
        this.mIsTrackerClosed = false;
        this.mIsTrackingStarted = false;
        this.mTokens = MAX_TOKENS;
        this.mIsThrottlingEnabled = true;
        this.mHandler = null;
        this.mModel = null;
    }

    Tracker(String trackingId, TrackerHandler handler) {
        this.mIsTrackerClosed = false;
        this.mIsTrackingStarted = false;
        this.mTokens = MAX_TOKENS;
        this.mIsThrottlingEnabled = true;
        if (trackingId == null) {
            throw new IllegalArgumentException("trackingId cannot be null");
        }
        this.mHandler = handler;
        this.mModel = new SimpleModel();
        this.mModel.set(ModelFields.TRACKING_ID, trackingId);
        this.mModel.set(ModelFields.SAMPLE_RATE, "100");
        this.mModel.setForNextHit(ModelFields.SESSION_CONTROL, "start");
        this.mModel.set(ModelFields.USE_SECURE, Boolean.toString(true));
    }

    private void assertTrackerOpen() {
        if (this.mIsTrackerClosed) {
            throw new IllegalStateException("Tracker closed");
        }
    }

    public void setStartSession(boolean startSession) {
        assertTrackerOpen();
        GAUsage.getInstance().setUsage(Field.SET_START_SESSION);
        this.mModel.setForNextHit(ModelFields.SESSION_CONTROL, startSession ? "start" : null);
    }

    public void setAppName(String appName) {
        if (this.mIsTrackingStarted) {
            Log.wDebug("Tracking already started, setAppName call ignored");
        } else if (TextUtils.isEmpty(appName)) {
            Log.wDebug("setting appName to empty value not allowed, call ignored");
        } else {
            GAUsage.getInstance().setUsage(Field.SET_APP_NAME);
            this.mModel.set(ModelFields.APP_NAME, appName);
        }
    }

    public void setAppVersion(String appVersion) {
        if (this.mIsTrackingStarted) {
            Log.wDebug("Tracking already started, setAppVersion call ignored");
            return;
        }
        GAUsage.getInstance().setUsage(Field.SET_APP_VERSION);
        this.mModel.set(ModelFields.APP_VERSION, appVersion);
    }

    public void setAppScreen(String appScreen) {
        assertTrackerOpen();
        GAUsage.getInstance().setUsage(Field.SET_APP_SCREEN);
        this.mModel.set(PlusShare.KEY_CONTENT_DEEP_LINK_METADATA_DESCRIPTION, appScreen);
    }

    @Deprecated
    public void trackView() {
        sendView();
    }

    public void sendView() {
        assertTrackerOpen();
        if (TextUtils.isEmpty(this.mModel.get(PlusShare.KEY_CONTENT_DEEP_LINK_METADATA_DESCRIPTION))) {
            throw new IllegalStateException("trackView requires a appScreen to be set");
        }
        GAUsage.getInstance().setUsage(Field.TRACK_VIEW);
        internalSend(ModelFields.APP_VIEW, null);
    }

    @Deprecated
    public void trackView(String appScreen) {
        sendView(appScreen);
    }

    public void sendView(String appScreen) {
        assertTrackerOpen();
        if (TextUtils.isEmpty(appScreen)) {
            throw new IllegalStateException("trackView requires a appScreen to be set");
        }
        GAUsage.getInstance().setUsage(Field.TRACK_VIEW_WITH_APPSCREEN);
        this.mModel.set(PlusShare.KEY_CONTENT_DEEP_LINK_METADATA_DESCRIPTION, appScreen);
        internalSend(ModelFields.APP_VIEW, null);
    }

    @Deprecated
    public void trackEvent(String category, String action, String label, Long value) {
        sendEvent(category, action, label, value);
    }

    public void sendEvent(String category, String action, String label, Long value) {
        assertTrackerOpen();
        GAUsage.getInstance().setUsage(Field.TRACK_EVENT);
        GAUsage.getInstance().setDisableUsage(true);
        internalSend(DataLayer.EVENT_KEY, constructEvent(category, action, label, value));
        GAUsage.getInstance().setDisableUsage(false);
    }

    @Deprecated
    public void trackTransaction(Transaction transaction) {
        sendTransaction(transaction);
    }

    public void sendTransaction(Transaction transaction) {
        assertTrackerOpen();
        GAUsage.getInstance().setUsage(Field.TRACK_TRANSACTION);
        GAUsage.getInstance().setDisableUsage(true);
        internalSend(ModelFields.TRANSACTION, constructTransaction(transaction));
        for (Item item : transaction.getItems()) {
            internalSend(ModelFields.ITEM, constructItem(item, transaction));
        }
        GAUsage.getInstance().setDisableUsage(false);
    }

    @Deprecated
    public void trackException(String description, boolean fatal) {
        sendException(description, fatal);
    }

    public void sendException(String description, boolean fatal) {
        assertTrackerOpen();
        GAUsage.getInstance().setUsage(Field.TRACK_EXCEPTION_WITH_DESCRIPTION);
        GAUsage.getInstance().setDisableUsage(true);
        internalSend(ModelFields.EXCEPTION, constructException(description, fatal));
        GAUsage.getInstance().setDisableUsage(false);
    }

    @Deprecated
    public void trackException(String threadName, Throwable exception, boolean fatal) {
        sendException(threadName, exception, fatal);
    }

    public void sendException(String threadName, Throwable exception, boolean fatal) {
        String description;
        assertTrackerOpen();
        GAUsage.getInstance().setUsage(Field.TRACK_EXCEPTION_WITH_THROWABLE);
        if (this.mExceptionParser != null) {
            description = this.mExceptionParser.getDescription(threadName, exception);
        } else {
            try {
                GAUsage.getInstance().setDisableUsage(true);
                internalSend(ModelFields.EXCEPTION, constructRawException(threadName, exception, fatal));
                GAUsage.getInstance().setDisableUsage(false);
                return;
            } catch (IOException e) {
                Log.w("trackException: couldn't serialize, sending \"Unknown Exception\"");
                description = "Unknown Exception";
            }
        }
        GAUsage.getInstance().setDisableUsage(true);
        sendException(description, fatal);
        GAUsage.getInstance().setDisableUsage(false);
    }

    @Deprecated
    public void trackTiming(String category, long intervalInMilliseconds, String name, String label) {
        sendTiming(category, intervalInMilliseconds, name, label);
    }

    public void sendTiming(String category, long intervalInMilliseconds, String name, String label) {
        assertTrackerOpen();
        GAUsage.getInstance().setUsage(Field.TRACK_TIMING);
        GAUsage.getInstance().setDisableUsage(true);
        internalSend(ModelFields.TIMING, constructTiming(category, intervalInMilliseconds, name, label));
        GAUsage.getInstance().setDisableUsage(false);
    }

    @Deprecated
    public void trackSocial(String network, String action, String target) {
        sendSocial(network, action, target);
    }

    public void sendSocial(String network, String action, String target) {
        assertTrackerOpen();
        GAUsage.getInstance().setUsage(Field.TRACK_SOCIAL);
        GAUsage.getInstance().setDisableUsage(true);
        internalSend(ModelFields.SOCIAL, constructSocial(network, action, target));
        GAUsage.getInstance().setDisableUsage(false);
    }

    public void close() {
        this.mIsTrackerClosed = true;
        this.mHandler.closeTracker(this);
    }

    public void send(String hitType, Map<String, String> params) {
        assertTrackerOpen();
        GAUsage.getInstance().setUsage(Field.SEND);
        internalSend(hitType, params);
    }

    private void internalSend(String hitType, Map<String, String> params) {
        this.mIsTrackingStarted = true;
        if (params == null) {
            params = new HashMap();
        }
        params.put(ModelFields.HIT_TYPE, hitType);
        this.mModel.setAll(params, Boolean.valueOf(true));
        if (tokensAvailable()) {
            this.mHandler.sendHit(this.mModel.getKeysAndValues());
        } else {
            Log.wDebug("Too many hits sent too quickly, throttling invoked.");
        }
        this.mModel.clearTemporaryValues();
    }

    public String get(String key) {
        GAUsage.getInstance().setUsage(Field.GET);
        return this.mModel.get(key);
    }

    public void set(String key, String value) {
        GAUsage.getInstance().setUsage(Field.SET);
        this.mModel.set(key, value);
    }

    public String getTrackingId() {
        GAUsage.getInstance().setUsage(Field.GET_TRACKING_ID);
        return this.mModel.get(ModelFields.TRACKING_ID);
    }

    public void setAnonymizeIp(boolean anonymizeIp) {
        GAUsage.getInstance().setUsage(Field.SET_ANONYMIZE_IP);
        this.mModel.set(ModelFields.ANONYMIZE_IP, Boolean.toString(anonymizeIp));
    }

    public boolean isAnonymizeIpEnabled() {
        GAUsage.getInstance().setUsage(Field.GET_ANONYMIZE_IP);
        return Utils.safeParseBoolean(this.mModel.get(ModelFields.ANONYMIZE_IP));
    }

    public void setSampleRate(double sampleRate) {
        GAUsage.getInstance().setUsage(Field.SET_SAMPLE_RATE);
        this.mModel.set(ModelFields.SAMPLE_RATE, Double.toString(sampleRate));
    }

    public double getSampleRate() {
        GAUsage.getInstance().setUsage(Field.GET_SAMPLE_RATE);
        return Utils.safeParseDouble(this.mModel.get(ModelFields.SAMPLE_RATE));
    }

    public void setUseSecure(boolean useSecure) {
        GAUsage.getInstance().setUsage(Field.SET_USE_SECURE);
        this.mModel.set(ModelFields.USE_SECURE, Boolean.toString(useSecure));
    }

    public boolean isUseSecure() {
        GAUsage.getInstance().setUsage(Field.GET_USE_SECURE);
        return Boolean.parseBoolean(this.mModel.get(ModelFields.USE_SECURE));
    }

    public void setReferrer(String referrer) {
        GAUsage.getInstance().setUsage(Field.SET_REFERRER);
        this.mModel.setForNextHit(ModelFields.REFERRER, referrer);
    }

    public void setCampaign(String campaign) {
        GAUsage.getInstance().setUsage(Field.SET_CAMPAIGN);
        this.mModel.setForNextHit(ModelFields.CAMPAIGN, campaign);
    }

    public void setAppId(String appId) {
        GAUsage.getInstance().setUsage(Field.SET_APP_ID);
        this.mModel.set(ModelFields.APP_ID, appId);
    }

    public String getAppId() {
        GAUsage.getInstance().setUsage(Field.GET_APP_ID);
        return this.mModel.get(ModelFields.APP_ID);
    }

    public void setAppInstallerId(String appInstallerId) {
        GAUsage.getInstance().setUsage(Field.SET_APP_INSTALLER_ID);
        this.mModel.set(ModelFields.APP_INSTALLER_ID, appInstallerId);
    }

    public String getAppInstallerId() {
        GAUsage.getInstance().setUsage(Field.GET_APP_INSTALLER_ID);
        return this.mModel.get(ModelFields.APP_INSTALLER_ID);
    }

    public void setExceptionParser(ExceptionParser exceptionParser) {
        GAUsage.getInstance().setUsage(Field.SET_EXCEPTION_PARSER);
        this.mExceptionParser = exceptionParser;
    }

    public ExceptionParser getExceptionParser() {
        GAUsage.getInstance().setUsage(Field.GET_EXCEPTION_PARSER);
        return this.mExceptionParser;
    }

    public void setCustomDimension(int index, String value) {
        if (index < 1) {
            Log.w("index must be > 0, ignoring setCustomDimension call for " + index + ", " + value);
        } else {
            this.mModel.setForNextHit(Utils.getSlottedModelField(ModelFields.CUSTOM_DIMENSION, index), value);
        }
    }

    public void setCustomMetric(int index, Long value) {
        if (index < 1) {
            Log.w("index must be > 0, ignoring setCustomMetric call for " + index + ", " + value);
            return;
        }
        String tmpValue = null;
        if (value != null) {
            tmpValue = Long.toString(value.longValue());
        }
        this.mModel.setForNextHit(Utils.getSlottedModelField(ModelFields.CUSTOM_METRIC, index), tmpValue);
    }

    public void setCustomDimensionsAndMetrics(Map<Integer, String> dimensions, Map<Integer, Long> metrics) {
        if (dimensions != null) {
            for (Integer key : dimensions.keySet()) {
                setCustomDimension(key.intValue(), (String) dimensions.get(key));
            }
        }
        if (metrics != null) {
            for (Integer key2 : metrics.keySet()) {
                setCustomMetric(key2.intValue(), (Long) metrics.get(key2));
            }
        }
    }

    public Map<String, String> constructEvent(String category, String action, String label, Long value) {
        Map<String, String> params = new HashMap();
        params.put(ModelFields.EVENT_CATEGORY, category);
        params.put(ModelFields.EVENT_ACTION, action);
        params.put(ModelFields.EVENT_LABEL, label);
        if (value != null) {
            params.put(ModelFields.EVENT_VALUE, Long.toString(value.longValue()));
        }
        GAUsage.getInstance().setUsage(Field.CONSTRUCT_EVENT);
        return params;
    }

    private static String microsToCurrencyString(long currencyInMicros) {
        return DF.format(((double) currencyInMicros) / 1000000.0d);
    }

    public Map<String, String> constructTransaction(Transaction trans) {
        Map<String, String> params = new HashMap();
        params.put(ModelFields.TRANSACTION_ID, trans.getTransactionId());
        params.put(ModelFields.TRANSACTION_AFFILIATION, trans.getAffiliation());
        params.put(ModelFields.TRANSACTION_SHIPPING, microsToCurrencyString(trans.getShippingCostInMicros()));
        params.put(ModelFields.TRANSACTION_TAX, microsToCurrencyString(trans.getTotalTaxInMicros()));
        params.put(ModelFields.TRANSACTION_TOTAL, microsToCurrencyString(trans.getTotalCostInMicros()));
        params.put("currencyCode", trans.getCurrencyCode());
        GAUsage.getInstance().setUsage(Field.CONSTRUCT_TRANSACTION);
        return params;
    }

    private Map<String, String> constructItem(Item item, Transaction trans) {
        Map<String, String> params = new HashMap();
        params.put(ModelFields.TRANSACTION_ID, trans.getTransactionId());
        params.put("currencyCode", trans.getCurrencyCode());
        params.put(ModelFields.ITEM_CODE, item.getSKU());
        params.put(ModelFields.ITEM_NAME, item.getName());
        params.put(ModelFields.ITEM_CATEGORY, item.getCategory());
        params.put(ModelFields.ITEM_PRICE, microsToCurrencyString(item.getPriceInMicros()));
        params.put(ModelFields.ITEM_QUANTITY, Long.toString(item.getQuantity()));
        GAUsage.getInstance().setUsage(Field.CONSTRUCT_ITEM);
        return params;
    }

    public Map<String, String> constructException(String exceptionDescription, boolean fatal) {
        Map<String, String> params = new HashMap();
        params.put(ModelFields.EX_DESCRIPTION, exceptionDescription);
        params.put(ModelFields.EX_FATAL, Boolean.toString(fatal));
        GAUsage.getInstance().setUsage(Field.CONSTRUCT_EXCEPTION);
        return params;
    }

    public Map<String, String> constructRawException(String threadName, Throwable exception, boolean fatal) throws IOException {
        Map<String, String> params = new HashMap();
        ByteArrayOutputStream byteStream = new ByteArrayOutputStream();
        ObjectOutputStream stream = new ObjectOutputStream(byteStream);
        stream.writeObject(exception);
        stream.close();
        params.put(ModelFields.RAW_EXCEPTION, Utils.hexEncode(byteStream.toByteArray()));
        if (threadName != null) {
            params.put(ModelFields.EXCEPTION_THREAD_NAME, threadName);
        }
        params.put(ModelFields.EX_FATAL, Boolean.toString(fatal));
        GAUsage.getInstance().setUsage(Field.CONSTRUCT_RAW_EXCEPTION);
        return params;
    }

    public Map<String, String> constructTiming(String category, long intervalInMilliseconds, String name, String label) {
        Map<String, String> params = new HashMap();
        params.put(ModelFields.TIMING_CATEGORY, category);
        params.put(ModelFields.TIMING_VALUE, Long.toString(intervalInMilliseconds));
        params.put(ModelFields.TIMING_VAR, name);
        params.put(ModelFields.TIMING_LABEL, label);
        GAUsage.getInstance().setUsage(Field.CONSTRUCT_TIMING);
        return params;
    }

    public Map<String, String> constructSocial(String network, String action, String target) {
        Map<String, String> params = new HashMap();
        params.put(ModelFields.SOCIAL_NETWORK, network);
        params.put(ModelFields.SOCIAL_ACTION, action);
        params.put(ModelFields.SOCIAL_TARGET, target);
        GAUsage.getInstance().setUsage(Field.CONSTRUCT_SOCIAL);
        return params;
    }

    @VisibleForTesting
    void setLastTrackTime(long lastTrackTime) {
        this.mLastTrackTime = lastTrackTime;
    }

    @VisibleForTesting
    void setTokens(long tokens) {
        this.mTokens = tokens;
    }

    @VisibleForTesting
    synchronized boolean tokensAvailable() {
        boolean z = true;
        synchronized (this) {
            if (this.mIsThrottlingEnabled) {
                long timeNow = System.currentTimeMillis();
                if (this.mTokens < MAX_TOKENS) {
                    long timeElapsed = timeNow - this.mLastTrackTime;
                    if (timeElapsed > 0) {
                        this.mTokens = Math.min(MAX_TOKENS, this.mTokens + timeElapsed);
                    }
                }
                this.mLastTrackTime = timeNow;
                if (this.mTokens >= TIME_PER_TOKEN_MILLIS) {
                    this.mTokens -= TIME_PER_TOKEN_MILLIS;
                } else {
                    Log.wDebug("Excessive tracking detected.  Tracking call ignored.");
                    z = false;
                }
            }
        }
        return z;
    }

    @VisibleForTesting
    public void setThrottlingEnabled(boolean throttlingEnabled) {
        this.mIsThrottlingEnabled = throttlingEnabled;
    }
}
