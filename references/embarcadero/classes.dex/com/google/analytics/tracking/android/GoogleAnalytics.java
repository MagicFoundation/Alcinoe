package com.google.analytics.tracking.android;

import android.content.Context;
import com.google.analytics.tracking.android.AnalyticsThread.ClientIdCallback;
import com.google.analytics.tracking.android.GAUsage.Field;
import com.google.android.gms.common.util.VisibleForTesting;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

public class GoogleAnalytics implements TrackerHandler {
    private static GoogleAnalytics sInstance;
    private AdHitIdGenerator mAdHitIdGenerator;
    private volatile Boolean mAppOptOut;
    private volatile String mClientId;
    private Context mContext;
    private boolean mDebug;
    private Tracker mDefaultTracker;
    private String mLastTrackingId;
    private AnalyticsThread mThread;
    private final Map<String, Tracker> mTrackers;

    public interface AppOptOutCallback {
        void reportAppOptOut(boolean z);
    }

    @VisibleForTesting
    GoogleAnalytics() {
        this.mTrackers = new HashMap();
    }

    private GoogleAnalytics(Context context) {
        this(context, GAThread.getInstance(context));
    }

    private GoogleAnalytics(Context context, AnalyticsThread thread) {
        this.mTrackers = new HashMap();
        if (context == null) {
            throw new IllegalArgumentException("context cannot be null");
        }
        this.mContext = context.getApplicationContext();
        this.mThread = thread;
        this.mAdHitIdGenerator = new AdHitIdGenerator();
        this.mThread.requestAppOptOut(new AppOptOutCallback() {
            public void reportAppOptOut(boolean optOut) {
                GoogleAnalytics.this.mAppOptOut = Boolean.valueOf(optOut);
            }
        });
        this.mThread.requestClientId(new ClientIdCallback() {
            public void reportClientId(String clientId) {
                GoogleAnalytics.this.mClientId = clientId;
            }
        });
    }

    public static GoogleAnalytics getInstance(Context context) {
        GoogleAnalytics googleAnalytics;
        synchronized (GoogleAnalytics.class) {
            if (sInstance == null) {
                sInstance = new GoogleAnalytics(context);
            }
            googleAnalytics = sInstance;
        }
        return googleAnalytics;
    }

    static GoogleAnalytics getInstance() {
        GoogleAnalytics googleAnalytics;
        synchronized (GoogleAnalytics.class) {
            googleAnalytics = sInstance;
        }
        return googleAnalytics;
    }

    @VisibleForTesting
    static GoogleAnalytics getNewInstance(Context context, AnalyticsThread thread) {
        GoogleAnalytics googleAnalytics;
        synchronized (GoogleAnalytics.class) {
            if (sInstance != null) {
                sInstance.close();
            }
            sInstance = new GoogleAnalytics(context, thread);
            googleAnalytics = sInstance;
        }
        return googleAnalytics;
    }

    @VisibleForTesting
    static void clearInstance() {
        synchronized (GoogleAnalytics.class) {
            sInstance = null;
        }
    }

    public void setDebug(boolean debug) {
        GAUsage.getInstance().setUsage(Field.SET_DEBUG);
        this.mDebug = debug;
        Log.setDebug(debug);
    }

    public boolean isDebugEnabled() {
        GAUsage.getInstance().setUsage(Field.GET_DEBUG);
        return this.mDebug;
    }

    public Tracker getTracker(String trackingId) {
        Tracker tracker;
        synchronized (this) {
            if (trackingId == null) {
                throw new IllegalArgumentException("trackingId cannot be null");
            }
            tracker = (Tracker) this.mTrackers.get(trackingId);
            if (tracker == null) {
                tracker = new Tracker(trackingId, this);
                this.mTrackers.put(trackingId, tracker);
                if (this.mDefaultTracker == null) {
                    this.mDefaultTracker = tracker;
                }
            }
            GAUsage.getInstance().setUsage(Field.GET_TRACKER);
        }
        return tracker;
    }

    public Tracker getDefaultTracker() {
        Tracker tracker;
        synchronized (this) {
            GAUsage.getInstance().setUsage(Field.GET_DEFAULT_TRACKER);
            tracker = this.mDefaultTracker;
        }
        return tracker;
    }

    public void setDefaultTracker(Tracker tracker) {
        synchronized (this) {
            GAUsage.getInstance().setUsage(Field.SET_DEFAULT_TRACKER);
            this.mDefaultTracker = tracker;
        }
    }

    public void closeTracker(Tracker tracker) {
        synchronized (this) {
            this.mTrackers.values().remove(tracker);
            if (tracker == this.mDefaultTracker) {
                this.mDefaultTracker = null;
            }
        }
    }

    public void sendHit(Map<String, String> hit) {
        synchronized (this) {
            if (hit == null) {
                throw new IllegalArgumentException("hit cannot be null");
            }
            hit.put(ModelFields.LANGUAGE, Utils.getLanguage(Locale.getDefault()));
            hit.put("adSenseAdMobHitId", Integer.toString(this.mAdHitIdGenerator.getAdHitId()));
            hit.put(ModelFields.SCREEN_RESOLUTION, this.mContext.getResources().getDisplayMetrics().widthPixels + "x" + this.mContext.getResources().getDisplayMetrics().heightPixels);
            hit.put("usage", GAUsage.getInstance().getAndClearSequence());
            GAUsage.getInstance().getAndClearUsage();
            this.mThread.sendHit(hit);
            this.mLastTrackingId = (String) hit.get(ModelFields.TRACKING_ID);
        }
    }

    @VisibleForTesting
    void close() {
    }

    String getTrackingIdForAds() {
        return this.mLastTrackingId;
    }

    String getClientIdForAds() {
        if (this.mClientId == null) {
            return null;
        }
        return this.mClientId.toString();
    }

    public void setAppOptOut(boolean optOut) {
        GAUsage.getInstance().setUsage(Field.SET_APP_OPT_OUT);
        this.mAppOptOut = Boolean.valueOf(optOut);
        this.mThread.setAppOptOut(optOut);
    }

    @VisibleForTesting
    Boolean getAppOptOut() {
        return this.mAppOptOut;
    }

    public void requestAppOptOut(AppOptOutCallback callback) {
        GAUsage.getInstance().setUsage(Field.REQUEST_APP_OPT_OUT);
        if (this.mAppOptOut != null) {
            callback.reportAppOptOut(this.mAppOptOut.booleanValue());
        } else {
            this.mThread.requestAppOptOut(callback);
        }
    }
}
