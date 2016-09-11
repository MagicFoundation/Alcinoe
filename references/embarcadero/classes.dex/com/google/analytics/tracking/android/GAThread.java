package com.google.analytics.tracking.android;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.support.v4.media.TransportMediator;
import android.support.v4.view.accessibility.AccessibilityNodeInfoCompat;
import android.text.TextUtils;
import com.google.analytics.tracking.android.AnalyticsThread.ClientIdCallback;
import com.google.analytics.tracking.android.GoogleAnalytics.AppOptOutCallback;
import com.google.android.gms.analytics.internal.Command;
import com.google.android.gms.common.util.VisibleForTesting;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.LinkedBlockingQueue;

class GAThread extends Thread implements AnalyticsThread {
    static final String API_VERSION = "1";
    private static final String CLIENT_VERSION = "ma1b5";
    private static final int MAX_SAMPLE_RATE = 100;
    private static final int SAMPLE_RATE_MODULO = 10000;
    private static final int SAMPLE_RATE_MULTIPLIER = 100;
    private static GAThread sInstance;
    private volatile boolean mAppOptOut;
    private volatile String mClientId;
    private volatile boolean mClosed;
    private volatile List<Command> mCommands;
    private final Context mContext;
    private volatile boolean mDisabled;
    private volatile String mInstallCampaign;
    private volatile MetaModel mMetaModel;
    private volatile ServiceProxy mServiceProxy;
    private final LinkedBlockingQueue<Runnable> queue;

    /* renamed from: com.google.analytics.tracking.android.GAThread.1 */
    class AnonymousClass1 implements Runnable {
        final /* synthetic */ Map val$hitCopy;
        final /* synthetic */ long val$hitTime;

        AnonymousClass1(Map map, long j) {
            this.val$hitCopy = map;
            this.val$hitTime = j;
        }

        public void run() {
            this.val$hitCopy.put(ModelFields.CLIENT_ID, GAThread.this.mClientId);
            if (!GAThread.this.mAppOptOut && !GAThread.this.isSampledOut(this.val$hitCopy)) {
                if (!TextUtils.isEmpty(GAThread.this.mInstallCampaign)) {
                    this.val$hitCopy.put(ModelFields.CAMPAIGN, GAThread.this.mInstallCampaign);
                    GAThread.this.mInstallCampaign = null;
                }
                GAThread.this.fillAppParameters(this.val$hitCopy);
                GAThread.this.fillCampaignParameters(this.val$hitCopy);
                GAThread.this.fillExceptionParameters(this.val$hitCopy);
                GAThread.this.mServiceProxy.putHit(HitBuilder.generateHitParams(GAThread.this.mMetaModel, this.val$hitCopy), this.val$hitTime, GAThread.this.getHostUrl(this.val$hitCopy), GAThread.this.mCommands);
            }
        }
    }

    /* renamed from: com.google.analytics.tracking.android.GAThread.3 */
    class AnonymousClass3 implements Runnable {
        final /* synthetic */ boolean val$appOptOut;

        AnonymousClass3(boolean z) {
            this.val$appOptOut = z;
        }

        public void run() {
            if (GAThread.this.mAppOptOut != this.val$appOptOut) {
                if (this.val$appOptOut) {
                    try {
                        GAThread.this.mContext.getFileStreamPath("gaOptOut").createNewFile();
                    } catch (IOException e) {
                        Log.w("Error creating optOut file.");
                    }
                    GAThread.this.mServiceProxy.clearHits();
                } else {
                    GAThread.this.mContext.deleteFile("gaOptOut");
                }
                GAThread.this.mAppOptOut = this.val$appOptOut;
            }
        }
    }

    /* renamed from: com.google.analytics.tracking.android.GAThread.4 */
    class AnonymousClass4 implements Runnable {
        final /* synthetic */ AppOptOutCallback val$callback;

        AnonymousClass4(AppOptOutCallback appOptOutCallback) {
            this.val$callback = appOptOutCallback;
        }

        public void run() {
            this.val$callback.reportAppOptOut(GAThread.this.mAppOptOut);
        }
    }

    /* renamed from: com.google.analytics.tracking.android.GAThread.5 */
    class AnonymousClass5 implements Runnable {
        final /* synthetic */ ClientIdCallback val$callback;

        AnonymousClass5(ClientIdCallback clientIdCallback) {
            this.val$callback = clientIdCallback;
        }

        public void run() {
            this.val$callback.reportClientId(GAThread.this.mClientId);
        }
    }

    static GAThread getInstance(Context ctx) {
        if (sInstance == null) {
            sInstance = new GAThread(ctx);
        }
        return sInstance;
    }

    private GAThread(Context ctx) {
        super("GAThread");
        this.queue = new LinkedBlockingQueue();
        this.mDisabled = false;
        this.mClosed = false;
        if (ctx != null) {
            this.mContext = ctx.getApplicationContext();
        } else {
            this.mContext = ctx;
        }
        start();
    }

    @VisibleForTesting
    GAThread(Context ctx, ServiceProxy proxy) {
        super("GAThread");
        this.queue = new LinkedBlockingQueue();
        this.mDisabled = false;
        this.mClosed = false;
        if (ctx != null) {
            this.mContext = ctx.getApplicationContext();
        } else {
            this.mContext = ctx;
        }
        this.mServiceProxy = proxy;
        start();
    }

    private void init() {
        this.mServiceProxy.createService();
        this.mCommands = new ArrayList();
        this.mCommands.add(new Command(Command.APPEND_VERSION, "_v", CLIENT_VERSION));
        this.mCommands.add(new Command(Command.APPEND_QUEUE_TIME, ModelFields.QUEUE_TIME, null));
        this.mCommands.add(new Command(Command.APPEND_CACHE_BUSTER, ModelFields.CACHE_BUSTER, null));
        this.mMetaModel = new MetaModel();
        MetaModelInitializer.set(this.mMetaModel);
    }

    public void sendHit(Map<String, String> hit) {
        Map<String, String> hitCopy = new HashMap(hit);
        long hitTime = System.currentTimeMillis();
        hitCopy.put("hitTime", Long.toString(hitTime));
        queueToThread(new AnonymousClass1(hitCopy, hitTime));
    }

    private String getHostUrl(Map<String, String> hit) {
        String hitUrl = (String) hit.get("internalHitUrl");
        if (hitUrl != null) {
            return hitUrl;
        }
        if (hit.containsKey(ModelFields.USE_SECURE)) {
            return Utils.safeParseBoolean((String) hit.get(ModelFields.USE_SECURE)) ? "https://ssl.google-analytics.com/collect" : "http://www.google-analytics.com/collect";
        } else {
            return "https://ssl.google-analytics.com/collect";
        }
    }

    private void fillExceptionParameters(Map<String, String> hit) {
        String rawExceptionString = (String) hit.get(ModelFields.RAW_EXCEPTION);
        if (rawExceptionString != null) {
            hit.remove(ModelFields.RAW_EXCEPTION);
            try {
                ObjectInputStream objectInputStream = new ObjectInputStream(new ByteArrayInputStream(Utils.hexDecode(rawExceptionString)));
                Object readObject = objectInputStream.readObject();
                objectInputStream.close();
                if (readObject instanceof Throwable) {
                    Throwable exception = (Throwable) readObject;
                    hit.put(ModelFields.EX_DESCRIPTION, new StandardExceptionParser(this.mContext, new ArrayList()).getDescription((String) hit.get(ModelFields.EXCEPTION_THREAD_NAME), exception));
                }
            } catch (IOException e) {
                Log.w("IOException reading exception");
            } catch (ClassNotFoundException e2) {
                Log.w("ClassNotFoundException reading exception");
            }
        }
    }

    private boolean isSampledOut(Map<String, String> hit) {
        if (hit.get(ModelFields.SAMPLE_RATE) != null) {
            double sampleRate = Utils.safeParseDouble((String) hit.get(ModelFields.SAMPLE_RATE));
            if (sampleRate <= 0.0d) {
                return true;
            }
            if (sampleRate < 100.0d) {
                String clientId = (String) hit.get(ModelFields.CLIENT_ID);
                if (clientId != null && ((double) (Math.abs(clientId.hashCode()) % SAMPLE_RATE_MODULO)) >= 100.0d * sampleRate) {
                    return true;
                }
            }
        }
        return false;
    }

    private void fillAppParameters(Map<String, String> hit) {
        PackageManager pm = this.mContext.getPackageManager();
        String appId = this.mContext.getPackageName();
        String appInstallerId = pm.getInstallerPackageName(appId);
        String appName = appId;
        String appVersion = null;
        try {
            PackageInfo packageInfo = pm.getPackageInfo(this.mContext.getPackageName(), 0);
            if (packageInfo != null) {
                appName = pm.getApplicationLabel(packageInfo.applicationInfo).toString();
                appVersion = packageInfo.versionName;
            }
        } catch (NameNotFoundException e) {
            Log.e("Error retrieving package info: appName set to " + appName);
        }
        putIfAbsent(hit, ModelFields.APP_NAME, appName);
        putIfAbsent(hit, ModelFields.APP_VERSION, appVersion);
        putIfAbsent(hit, ModelFields.APP_ID, appId);
        putIfAbsent(hit, ModelFields.APP_INSTALLER_ID, appInstallerId);
        hit.put(ModelFields.API_VERSION, API_VERSION);
    }

    private void putIfAbsent(Map<String, String> hit, String key, String value) {
        if (!hit.containsKey(key)) {
            hit.put(key, value);
        }
    }

    private void fillCampaignParameters(Map<String, String> hit) {
        String campaign = Utils.filterCampaign((String) hit.get(ModelFields.CAMPAIGN));
        if (!TextUtils.isEmpty(campaign)) {
            Map<String, String> paramsMap = Utils.parseURLParameters(campaign);
            hit.put(ModelFields.CAMPAIGN_CONTENT, paramsMap.get("utm_content"));
            hit.put(ModelFields.CAMPAIGN_MEDIUM, paramsMap.get("utm_medium"));
            hit.put(ModelFields.CAMPAIGN_NAME, paramsMap.get("utm_campaign"));
            hit.put(ModelFields.CAMPAIGN_SOURCE, paramsMap.get("utm_source"));
            hit.put(ModelFields.CAMPAIGN_KEYWORD, paramsMap.get("utm_term"));
            hit.put(ModelFields.CAMPAIGN_ID, paramsMap.get("utm_id"));
            hit.put(ModelFields.GCLID, paramsMap.get(ModelFields.GCLID));
            hit.put(ModelFields.DCLID, paramsMap.get(ModelFields.DCLID));
            hit.put(ModelFields.GMOB_T, paramsMap.get(ModelFields.GMOB_T));
        }
    }

    public void dispatch() {
        queueToThread(new Runnable() {
            public void run() {
                GAThread.this.mServiceProxy.dispatch();
            }
        });
    }

    public void setAppOptOut(boolean appOptOut) {
        queueToThread(new AnonymousClass3(appOptOut));
    }

    public void requestAppOptOut(AppOptOutCallback callback) {
        queueToThread(new AnonymousClass4(callback));
    }

    public void requestClientId(ClientIdCallback callback) {
        queueToThread(new AnonymousClass5(callback));
    }

    private void queueToThread(Runnable r) {
        this.queue.add(r);
    }

    private boolean loadAppOptOut() {
        return this.mContext.getFileStreamPath("gaOptOut").exists();
    }

    private boolean storeClientId(String clientId) {
        try {
            FileOutputStream fos = this.mContext.openFileOutput("gaClientId", 0);
            fos.write(clientId.getBytes());
            fos.close();
            return true;
        } catch (FileNotFoundException e) {
            Log.e("Error creating clientId file.");
            return false;
        } catch (IOException e2) {
            Log.e("Error writing to clientId file.");
            return false;
        }
    }

    private String generateClientId() {
        String result = UUID.randomUUID().toString().toLowerCase();
        if (storeClientId(result)) {
            return result;
        }
        return "0";
    }

    @VisibleForTesting
    String initializeClientId() {
        String rslt = null;
        try {
            FileInputStream input = this.mContext.openFileInput("gaClientId");
            byte[] bytes = new byte[TransportMediator.FLAG_KEY_MEDIA_NEXT];
            int readLen = input.read(bytes, 0, TransportMediator.FLAG_KEY_MEDIA_NEXT);
            if (input.available() > 0) {
                Log.e("clientId file seems corrupted, deleting it.");
                input.close();
                this.mContext.deleteFile("gaInstallData");
            }
            if (readLen <= 0) {
                Log.e("clientId file seems empty, deleting it.");
                input.close();
                this.mContext.deleteFile("gaInstallData");
            } else {
                String rslt2 = new String(bytes, 0, readLen);
                try {
                    input.close();
                    rslt = rslt2;
                } catch (FileNotFoundException e) {
                    rslt = rslt2;
                } catch (IOException e2) {
                    rslt = rslt2;
                    Log.e("Error reading clientId file, deleting it.");
                    this.mContext.deleteFile("gaInstallData");
                } catch (NumberFormatException e3) {
                    rslt = rslt2;
                    Log.e("cliendId file doesn't have long value, deleting it.");
                    this.mContext.deleteFile("gaInstallData");
                }
            }
        } catch (FileNotFoundException e4) {
        } catch (IOException e5) {
            Log.e("Error reading clientId file, deleting it.");
            this.mContext.deleteFile("gaInstallData");
        } catch (NumberFormatException e6) {
            Log.e("cliendId file doesn't have long value, deleting it.");
            this.mContext.deleteFile("gaInstallData");
        }
        if (rslt == null) {
            return generateClientId();
        }
        return rslt;
    }

    @VisibleForTesting
    static String getAndClearCampaign(Context context) {
        try {
            FileInputStream input = context.openFileInput("gaInstallData");
            byte[] inputBytes = new byte[AccessibilityNodeInfoCompat.ACTION_SCROLL_BACKWARD];
            int readLen = input.read(inputBytes, 0, AccessibilityNodeInfoCompat.ACTION_SCROLL_BACKWARD);
            if (input.available() > 0) {
                Log.e("Too much campaign data, ignoring it.");
                input.close();
                context.deleteFile("gaInstallData");
                return null;
            }
            input.close();
            context.deleteFile("gaInstallData");
            if (readLen <= 0) {
                Log.w("Campaign file is empty.");
                return null;
            }
            String campaignString = new String(inputBytes, 0, readLen);
            Log.i("Campaign found: " + campaignString);
            return campaignString;
        } catch (FileNotFoundException e) {
            Log.i("No campaign data found.");
            return null;
        } catch (IOException e2) {
            Log.e("Error reading campaign data.");
            context.deleteFile("gaInstallData");
            return null;
        }
    }

    private String printStackTrace(Throwable t) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream stream = new PrintStream(baos);
        t.printStackTrace(stream);
        stream.flush();
        return new String(baos.toByteArray());
    }

    public void run() {
        try {
            Thread.sleep(5000);
        } catch (InterruptedException e) {
            Log.w("sleep interrupted in GAThread initialize");
        }
        if (this.mServiceProxy == null) {
            this.mServiceProxy = new GAServiceProxy(this.mContext, this);
        }
        init();
        try {
            this.mAppOptOut = loadAppOptOut();
            this.mClientId = initializeClientId();
            this.mInstallCampaign = getAndClearCampaign(this.mContext);
        } catch (Throwable t) {
            Log.e("Error initializing the GAThread: " + printStackTrace(t));
            Log.e("Google Analytics will not start up.");
            this.mDisabled = true;
        }
        while (!this.mClosed) {
            try {
                Runnable r = (Runnable) this.queue.take();
                if (!this.mDisabled) {
                    r.run();
                }
            } catch (InterruptedException e2) {
                Log.i(e2.toString());
            } catch (Throwable t2) {
                Log.e("Error on GAThread: " + printStackTrace(t2));
                Log.e("Google Analytics is shutting down.");
                this.mDisabled = true;
            }
        }
    }

    public LinkedBlockingQueue<Runnable> getQueue() {
        return this.queue;
    }

    public Thread getThread() {
        return this;
    }

    @VisibleForTesting
    void close() {
        this.mClosed = true;
        interrupt();
    }

    @VisibleForTesting
    boolean isDisabled() {
        return this.mDisabled;
    }
}
