package com.google.analytics.tracking.android;

import android.app.Activity;
import android.content.Context;
import android.text.TextUtils;
import com.google.android.gms.common.util.VisibleForTesting;
import java.lang.Thread.UncaughtExceptionHandler;
import java.util.HashMap;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

public class EasyTracker {
    static final int NUM_MILLISECONDS_TO_WAIT_FOR_OPEN_ACTIVITY = 1000;
    private static EasyTracker sInstance;
    private int mActivitiesActive;
    private final Map<String, String> mActivityNameMap;
    private GoogleAnalytics mAnalyticsInstance;
    private String mAppName;
    private String mAppVersion;
    private Clock mClock;
    private Context mContext;
    private boolean mDebug;
    private int mDispatchPeriod;
    private UncaughtExceptionHandler mExceptionHandler;
    private boolean mIsAnonymizeIpEnabled;
    private boolean mIsAutoActivityTracking;
    private boolean mIsEnabled;
    private boolean mIsInForeground;
    private boolean mIsReportUncaughtExceptionsEnabled;
    private long mLastOnStopTime;
    private ParameterLoader mParameterFetcher;
    private Double mSampleRate;
    private ServiceManager mServiceManager;
    private long mSessionTimeout;
    private Timer mTimer;
    private TimerTask mTimerTask;
    private Tracker mTracker;
    private String mTrackingId;

    private class NotInForegroundTimerTask extends TimerTask {
        private NotInForegroundTimerTask() {
        }

        public void run() {
            EasyTracker.this.mIsInForeground = false;
        }
    }

    class NoopTracker extends Tracker {
        private String mAppId;
        private String mAppInstallerId;
        private ExceptionParser mExceptionParser;
        private boolean mIsAnonymizeIp;
        private boolean mIsUseSecure;
        private double mSampleRate;

        NoopTracker() {
            this.mSampleRate = 100.0d;
        }

        public void setStartSession(boolean startSession) {
        }

        public void setAppName(String appName) {
        }

        public void setAppVersion(String appVersion) {
        }

        public void setAppScreen(String appScreen) {
        }

        public void sendView() {
        }

        public void sendView(String appScreen) {
        }

        public void sendEvent(String category, String action, String label, Long value) {
        }

        public void sendTransaction(Transaction transaction) {
        }

        public void sendException(String description, boolean fatal) {
        }

        public void sendException(String threadName, Throwable exception, boolean fatal) {
        }

        public void sendTiming(String category, long intervalInMilliseconds, String name, String label) {
        }

        public void sendSocial(String network, String action, String target) {
        }

        public void close() {
        }

        public void send(String hitType, Map<String, String> map) {
        }

        public String get(String key) {
            return "";
        }

        public void set(String key, String value) {
        }

        public String getTrackingId() {
            return "";
        }

        public void setAnonymizeIp(boolean anonymizeIp) {
            this.mIsAnonymizeIp = anonymizeIp;
        }

        public boolean isAnonymizeIpEnabled() {
            return this.mIsAnonymizeIp;
        }

        public void setSampleRate(double sampleRate) {
            this.mSampleRate = sampleRate;
        }

        public double getSampleRate() {
            return this.mSampleRate;
        }

        public void setUseSecure(boolean useSecure) {
            this.mIsUseSecure = useSecure;
        }

        public boolean isUseSecure() {
            return this.mIsUseSecure;
        }

        public void setReferrer(String referrer) {
        }

        public void setCampaign(String campaign) {
        }

        public void setAppId(String appId) {
            this.mAppId = appId;
        }

        public String getAppId() {
            return this.mAppId;
        }

        public void setAppInstallerId(String appInstallerId) {
            this.mAppInstallerId = appInstallerId;
        }

        public String getAppInstallerId() {
            return this.mAppInstallerId;
        }

        public void setExceptionParser(ExceptionParser exceptionParser) {
            this.mExceptionParser = exceptionParser;
        }

        public ExceptionParser getExceptionParser() {
            return this.mExceptionParser;
        }

        public Map<String, String> constructEvent(String category, String action, String label, Long value) {
            return new HashMap();
        }

        public Map<String, String> constructTransaction(Transaction trans) {
            return new HashMap();
        }

        public Map<String, String> constructException(String exceptionDescription, boolean fatal) {
            return new HashMap();
        }

        public Map<String, String> constructRawException(String threadName, Throwable exception, boolean fatal) {
            return new HashMap();
        }

        public Map<String, String> constructTiming(String category, long intervalInMilliseconds, String name, String label) {
            return new HashMap();
        }

        public Map<String, String> constructSocial(String network, String action, String target) {
            return new HashMap();
        }

        public void setCustomDimension(int slot, String value) {
        }

        public void setCustomMetric(int slot, Long value) {
        }

        public void setCustomDimensionsAndMetrics(Map<Integer, String> map, Map<Integer, Long> map2) {
        }
    }

    private EasyTracker() {
        this.mIsEnabled = false;
        this.mDispatchPeriod = 1800;
        this.mIsAutoActivityTracking = false;
        this.mActivitiesActive = 0;
        this.mActivityNameMap = new HashMap();
        this.mTracker = null;
        this.mIsInForeground = false;
        this.mClock = new Clock() {
            public long currentTimeMillis() {
                return System.currentTimeMillis();
            }
        };
    }

    public static EasyTracker getInstance() {
        if (sInstance == null) {
            sInstance = new EasyTracker();
        }
        return sInstance;
    }

    public static Tracker getTracker() {
        if (getInstance().mContext != null) {
            return getInstance().mTracker;
        }
        throw new IllegalStateException("You must call EasyTracker.getInstance().setContext(context) or startActivity(activity) before calling getTracker()");
    }

    boolean checkForNewSession() {
        return this.mSessionTimeout == 0 || (this.mSessionTimeout > 0 && this.mClock.currentTimeMillis() > this.mLastOnStopTime + this.mSessionTimeout);
    }

    private void loadParameters() {
        boolean z = true;
        this.mTrackingId = this.mParameterFetcher.getString("ga_trackingId");
        if (TextUtils.isEmpty(this.mTrackingId)) {
            this.mTrackingId = this.mParameterFetcher.getString("ga_api_key");
            if (TextUtils.isEmpty(this.mTrackingId)) {
                Log.e("EasyTracker requested, but missing required ga_trackingId");
                this.mTracker = new NoopTracker();
                return;
            }
        }
        this.mIsEnabled = true;
        this.mAppName = this.mParameterFetcher.getString("ga_appName");
        this.mAppVersion = this.mParameterFetcher.getString("ga_appVersion");
        this.mDebug = this.mParameterFetcher.getBoolean("ga_debug");
        this.mSampleRate = this.mParameterFetcher.getDoubleFromString("ga_sampleFrequency");
        if (this.mSampleRate == null) {
            this.mSampleRate = new Double((double) this.mParameterFetcher.getInt("ga_sampleRate", 100));
        }
        this.mDispatchPeriod = this.mParameterFetcher.getInt("ga_dispatchPeriod", 1800);
        this.mSessionTimeout = (long) (this.mParameterFetcher.getInt("ga_sessionTimeout", 30) * NUM_MILLISECONDS_TO_WAIT_FOR_OPEN_ACTIVITY);
        if (!(this.mParameterFetcher.getBoolean("ga_autoActivityTracking") || this.mParameterFetcher.getBoolean("ga_auto_activity_tracking"))) {
            z = false;
        }
        this.mIsAutoActivityTracking = z;
        this.mIsAnonymizeIpEnabled = this.mParameterFetcher.getBoolean("ga_anonymizeIp");
        this.mIsReportUncaughtExceptionsEnabled = this.mParameterFetcher.getBoolean("ga_reportUncaughtExceptions");
        this.mTracker = this.mAnalyticsInstance.getTracker(this.mTrackingId);
        if (!TextUtils.isEmpty(this.mAppName)) {
            Log.i("setting appName to " + this.mAppName);
            this.mTracker.setAppName(this.mAppName);
        }
        if (this.mAppVersion != null) {
            this.mTracker.setAppVersion(this.mAppVersion);
        }
        this.mTracker.setAnonymizeIp(this.mIsAnonymizeIpEnabled);
        this.mTracker.setSampleRate(this.mSampleRate.doubleValue());
        this.mAnalyticsInstance.setDebug(this.mDebug);
        this.mServiceManager.setDispatchPeriod(this.mDispatchPeriod);
        if (this.mIsReportUncaughtExceptionsEnabled) {
            UncaughtExceptionHandler newHandler = this.mExceptionHandler;
            if (newHandler == null) {
                newHandler = new ExceptionReporter(this.mTracker, this.mServiceManager, Thread.getDefaultUncaughtExceptionHandler(), this.mContext);
            }
            Thread.setDefaultUncaughtExceptionHandler(newHandler);
        }
    }

    @VisibleForTesting
    void setUncaughtExceptionHandler(UncaughtExceptionHandler handler) {
        this.mExceptionHandler = handler;
    }

    public void setContext(Context ctx) {
        if (ctx == null) {
            Log.e("Context cannot be null");
            return;
        }
        setContext(ctx, new ParameterLoaderImpl(ctx.getApplicationContext()), GoogleAnalytics.getInstance(ctx.getApplicationContext()), GAServiceManager.getInstance());
    }

    @VisibleForTesting
    void setContext(Context ctx, ParameterLoader parameterLoader, GoogleAnalytics ga, ServiceManager serviceManager) {
        if (ctx == null) {
            Log.e("Context cannot be null");
        }
        if (this.mContext == null) {
            this.mContext = ctx.getApplicationContext();
            this.mAnalyticsInstance = ga;
            this.mServiceManager = serviceManager;
            this.mParameterFetcher = parameterLoader;
            loadParameters();
        }
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    public void activityStart(Activity activity) {
        setContext(activity);
        if (this.mIsEnabled) {
            clearExistingTimer();
            if (!this.mIsInForeground && this.mActivitiesActive == 0 && checkForNewSession()) {
                this.mTracker.setStartSession(true);
            }
            this.mIsInForeground = true;
            this.mActivitiesActive++;
            if (this.mIsAutoActivityTracking) {
                this.mTracker.sendView(getActivityName(activity));
            }
        }
    }

    public void activityStop(Activity activity) {
        setContext(activity);
        if (this.mIsEnabled) {
            this.mActivitiesActive--;
            this.mActivitiesActive = Math.max(0, this.mActivitiesActive);
            this.mLastOnStopTime = this.mClock.currentTimeMillis();
            if (this.mActivitiesActive == 0) {
                clearExistingTimer();
                this.mTimerTask = new NotInForegroundTimerTask();
                this.mTimer = new Timer("waitForActivityStart");
                this.mTimer.schedule(this.mTimerTask, 1000);
            }
        }
    }

    public void dispatch() {
        if (this.mIsEnabled) {
            this.mServiceManager.dispatch();
        }
    }

    private synchronized void clearExistingTimer() {
        if (this.mTimer != null) {
            this.mTimer.cancel();
            this.mTimer = null;
        }
    }

    private String getActivityName(Activity activity) {
        String canonicalName = activity.getClass().getCanonicalName();
        if (this.mActivityNameMap.containsKey(canonicalName)) {
            return (String) this.mActivityNameMap.get(canonicalName);
        }
        String name = this.mParameterFetcher.getString(canonicalName);
        if (name == null) {
            name = canonicalName;
        }
        this.mActivityNameMap.put(canonicalName, name);
        return name;
    }

    @VisibleForTesting
    static void clearTracker() {
        sInstance = null;
    }

    @VisibleForTesting
    void setClock(Clock clock) {
        this.mClock = clock;
    }

    @VisibleForTesting
    int getActivitiesActive() {
        return this.mActivitiesActive;
    }
}
