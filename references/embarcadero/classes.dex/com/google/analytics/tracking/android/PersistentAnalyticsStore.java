package com.google.analytics.tracking.android;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteCursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteException;
import android.database.sqlite.SQLiteOpenHelper;
import android.os.Build.VERSION;
import android.text.TextUtils;
import com.google.android.gms.analytics.internal.Command;
import com.google.android.gms.common.util.VisibleForTesting;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.http.client.HttpClient;
import org.apache.http.impl.client.DefaultHttpClient;

class PersistentAnalyticsStore implements AnalyticsStore {
    @VisibleForTesting
    static final String BACKEND_LIBRARY_VERSION = "";
    private static final String CREATE_HITS_TABLE;
    private static final String DATABASE_FILENAME = "google_analytics_v2.db";
    @VisibleForTesting
    static final String HITS_TABLE = "hits2";
    @VisibleForTesting
    static final String HIT_APP_ID = "hit_app_id";
    @VisibleForTesting
    static final String HIT_ID = "hit_id";
    @VisibleForTesting
    static final String HIT_STRING = "hit_string";
    @VisibleForTesting
    static final String HIT_TIME = "hit_time";
    @VisibleForTesting
    static final String HIT_URL = "hit_url";
    private Clock mClock;
    private final Context mContext;
    private final String mDatabaseName;
    private final AnalyticsDatabaseHelper mDbHelper;
    private volatile Dispatcher mDispatcher;
    private long mLastDeleteStaleHitsTime;
    private final AnalyticsStoreStateListener mListener;

    @VisibleForTesting
    class AnalyticsDatabaseHelper extends SQLiteOpenHelper {
        private boolean mBadDatabase;
        private long mLastDatabaseCheckTime;

        boolean isBadDatabase() {
            return this.mBadDatabase;
        }

        void setBadDatabase(boolean badDatabase) {
            this.mBadDatabase = badDatabase;
        }

        AnalyticsDatabaseHelper(Context context, String databaseName) {
            super(context, databaseName, null, 1);
            this.mLastDatabaseCheckTime = 0;
        }

        private boolean tablePresent(String table, SQLiteDatabase db) {
            Cursor cursor = null;
            try {
                SQLiteDatabase sQLiteDatabase = db;
                cursor = sQLiteDatabase.query("SQLITE_MASTER", new String[]{"name"}, "name=?", new String[]{table}, null, null, null);
                boolean moveToFirst = cursor.moveToFirst();
                if (cursor == null) {
                    return moveToFirst;
                }
                cursor.close();
                return moveToFirst;
            } catch (SQLiteException e) {
                Log.w("error querying for table " + table);
                return false;
            } finally {
                if (cursor != null) {
                    cursor.close();
                }
            }
        }

        public SQLiteDatabase getWritableDatabase() {
            if (!this.mBadDatabase || this.mLastDatabaseCheckTime + 3600000 <= PersistentAnalyticsStore.this.mClock.currentTimeMillis()) {
                SQLiteDatabase db = null;
                this.mBadDatabase = true;
                this.mLastDatabaseCheckTime = PersistentAnalyticsStore.this.mClock.currentTimeMillis();
                try {
                    db = super.getWritableDatabase();
                } catch (SQLiteException e) {
                    PersistentAnalyticsStore.this.mContext.getDatabasePath(PersistentAnalyticsStore.this.mDatabaseName).delete();
                }
                if (db == null) {
                    db = super.getWritableDatabase();
                }
                this.mBadDatabase = false;
                return db;
            }
            throw new SQLiteException("Database creation failed");
        }

        public void onOpen(SQLiteDatabase db) {
            if (VERSION.SDK_INT < 15) {
                Cursor cursor = db.rawQuery("PRAGMA journal_mode=memory", null);
                try {
                    cursor.moveToFirst();
                } finally {
                    cursor.close();
                }
            }
            if (tablePresent(PersistentAnalyticsStore.HITS_TABLE, db)) {
                validateColumnsPresent(db);
            } else {
                db.execSQL(PersistentAnalyticsStore.CREATE_HITS_TABLE);
            }
        }

        private void validateColumnsPresent(SQLiteDatabase db) {
            Cursor c = db.rawQuery("SELECT * FROM hits2 WHERE 0", null);
            Set<String> columns = new HashSet();
            try {
                String[] columnNames = c.getColumnNames();
                for (Object add : columnNames) {
                    columns.add(add);
                }
                if (columns.remove(PersistentAnalyticsStore.HIT_ID) && columns.remove(PersistentAnalyticsStore.HIT_URL) && columns.remove(PersistentAnalyticsStore.HIT_STRING) && columns.remove(PersistentAnalyticsStore.HIT_TIME)) {
                    boolean needsAppId = !columns.remove(PersistentAnalyticsStore.HIT_APP_ID);
                    if (!columns.isEmpty()) {
                        throw new SQLiteException("Database has extra columns");
                    } else if (needsAppId) {
                        db.execSQL("ALTER TABLE hits2 ADD COLUMN hit_app_id");
                        return;
                    } else {
                        return;
                    }
                }
                throw new SQLiteException("Database column missing");
            } finally {
                c.close();
            }
        }

        public void onCreate(SQLiteDatabase db) {
            FutureApis.setOwnerOnlyReadWrite(db.getPath());
        }

        public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        }
    }

    static {
        CREATE_HITS_TABLE = String.format("CREATE TABLE IF NOT EXISTS %s ( '%s' INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, '%s' INTEGER NOT NULL, '%s' TEXT NOT NULL, '%s' TEXT NOT NULL, '%s' INTEGER);", new Object[]{HITS_TABLE, HIT_ID, HIT_TIME, HIT_URL, HIT_STRING, HIT_APP_ID});
    }

    PersistentAnalyticsStore(AnalyticsStoreStateListener listener, Context ctx) {
        this(listener, ctx, DATABASE_FILENAME);
    }

    @VisibleForTesting
    PersistentAnalyticsStore(AnalyticsStoreStateListener listener, Context ctx, String databaseName) {
        this.mContext = ctx.getApplicationContext();
        this.mDatabaseName = databaseName;
        this.mListener = listener;
        this.mClock = new Clock() {
            public long currentTimeMillis() {
                return System.currentTimeMillis();
            }
        };
        this.mDbHelper = new AnalyticsDatabaseHelper(this.mContext, this.mDatabaseName);
        this.mDispatcher = new SimpleNetworkDispatcher(this, createDefaultHttpClientFactory(), this.mContext);
        this.mLastDeleteStaleHitsTime = 0;
    }

    @VisibleForTesting
    public void setClock(Clock clock) {
        this.mClock = clock;
    }

    @VisibleForTesting
    public AnalyticsDatabaseHelper getDbHelper() {
        return this.mDbHelper;
    }

    private HttpClientFactory createDefaultHttpClientFactory() {
        return new HttpClientFactory() {
            public HttpClient newInstance() {
                return new DefaultHttpClient();
            }
        };
    }

    public void setDispatch(boolean dispatch) {
        this.mDispatcher = dispatch ? new SimpleNetworkDispatcher(this, createDefaultHttpClientFactory(), this.mContext) : new NoopDispatcher();
    }

    @VisibleForTesting
    void setDispatcher(Dispatcher dispatcher) {
        this.mDispatcher = dispatcher;
    }

    public void clearHits(long appId) {
        boolean z = true;
        SQLiteDatabase db = getWritableDatabase("Error opening database for clearHits");
        if (db != null) {
            if (appId == 0) {
                db.delete(HITS_TABLE, null, null);
            } else {
                db.delete(HITS_TABLE, "hit_app_id = ?", new String[]{Long.valueOf(appId).toString()});
            }
            AnalyticsStoreStateListener analyticsStoreStateListener = this.mListener;
            if (getNumStoredHits() != 0) {
                z = false;
            }
            analyticsStoreStateListener.reportStoreIsEmpty(z);
        }
    }

    public void putHit(Map<String, String> wireFormatParams, long hitTimeInMilliseconds, String path, Collection<Command> commands) {
        deleteStaleHits();
        fillVersionParametersIfNecessary(wireFormatParams, commands);
        removeOldHitIfFull();
        writeHitToDatabase(wireFormatParams, hitTimeInMilliseconds, path);
    }

    private void fillVersionParametersIfNecessary(Map<String, String> wireFormatParams, Collection<Command> commands) {
        for (Command command : commands) {
            if (command.getId().equals(Command.APPEND_VERSION)) {
                storeVersion(wireFormatParams, command.getUrlParam(), command.getValue());
                return;
            }
        }
    }

    private void storeVersion(Map<String, String> wireFormatParams, String versionUrlParam, String clientVersion) {
        String version = clientVersion;
        if (clientVersion == null) {
            version = BACKEND_LIBRARY_VERSION;
        } else {
            version = clientVersion + BACKEND_LIBRARY_VERSION;
        }
        if (versionUrlParam != null) {
            wireFormatParams.put(versionUrlParam, version);
        }
    }

    private void removeOldHitIfFull() {
        int hitsOverLimit = (getNumStoredHits() - 2000) + 1;
        if (hitsOverLimit > 0) {
            List<Hit> hitsToDelete = peekHits(hitsOverLimit);
            Log.wDebug("Store full, deleting " + hitsToDelete.size() + " hits to make room");
            deleteHits(hitsToDelete);
        }
    }

    private void writeHitToDatabase(Map<String, String> hit, long hitTimeInMilliseconds, String path) {
        SQLiteDatabase db = getWritableDatabase("Error opening database for putHit");
        if (db != null) {
            ContentValues content = new ContentValues();
            content.put(HIT_STRING, generateHitString(hit));
            content.put(HIT_TIME, Long.valueOf(hitTimeInMilliseconds));
            long appSystemId = 0;
            if (hit.containsKey(ModelFields.ANDROID_APP_UID)) {
                try {
                    appSystemId = Long.parseLong((String) hit.get(ModelFields.ANDROID_APP_UID));
                } catch (NumberFormatException e) {
                }
            }
            content.put(HIT_APP_ID, Long.valueOf(appSystemId));
            if (path == null) {
                path = "http://www.google-analytics.com/collect";
            }
            if (path.length() == 0) {
                Log.w("empty path: not sending hit");
                return;
            }
            content.put(HIT_URL, path);
            try {
                db.insert(HITS_TABLE, null, content);
                this.mListener.reportStoreIsEmpty(false);
            } catch (SQLiteException e2) {
                Log.w("Error storing hit");
            }
        }
    }

    public static String generateHitString(Map<String, String> urlParams) {
        List<String> keyAndValues = new ArrayList(urlParams.size());
        for (Entry<String, String> entry : urlParams.entrySet()) {
            keyAndValues.add(((String) entry.getKey()) + "=" + HitBuilder.encode((String) entry.getValue()));
        }
        return TextUtils.join("&", keyAndValues);
    }

    public List<Hit> peekHits(int maxHits) {
        Hit hit;
        SQLiteException e;
        Throwable th;
        SQLiteDatabase db = getWritableDatabase("Error opening database for peekHits");
        if (db == null) {
            return new ArrayList();
        }
        Cursor cursor = null;
        List<Hit> hits = new ArrayList();
        List<Hit> hits2;
        try {
            cursor = db.query(HITS_TABLE, new String[]{HIT_ID, HIT_TIME, HIT_URL}, null, null, null, null, String.format("%s ASC, %s ASC", new Object[]{HIT_URL, HIT_ID}), Integer.toString(maxHits));
            hits2 = new ArrayList();
            try {
                if (cursor.moveToFirst()) {
                    do {
                        hit = new Hit(null, cursor.getLong(0), cursor.getLong(1));
                        hit.setHitUrl(cursor.getString(2));
                        hits2.add(hit);
                    } while (cursor.moveToNext());
                }
                if (cursor != null) {
                    cursor.close();
                }
                int count = 0;
                SQLiteDatabase sQLiteDatabase = db;
                cursor = sQLiteDatabase.query(HITS_TABLE, new String[]{HIT_ID, HIT_STRING}, null, null, null, null, String.format("%s ASC", new Object[]{HIT_ID}), Integer.toString(maxHits));
                if (cursor.moveToFirst()) {
                    do {
                        if (cursor instanceof SQLiteCursor) {
                            if (((SQLiteCursor) cursor).getWindow().getNumRows() > 0) {
                                ((Hit) hits2.get(count)).setHitString(cursor.getString(1));
                            } else {
                                try {
                                    Log.w("hitString for hitId " + ((Hit) hits2.get(count)).getHitId() + " too large.  Hit will be deleted.");
                                } catch (SQLiteException e2) {
                                    Log.w("error in peekHits fetching hitString: " + e2.getMessage());
                                    List<Hit> partialHits = new ArrayList();
                                    boolean foundOneBadHit = false;
                                    for (Hit hit2 : hits2) {
                                        if (TextUtils.isEmpty(hit2.getHitParams())) {
                                            if (foundOneBadHit) {
                                                break;
                                            }
                                            foundOneBadHit = true;
                                        }
                                        partialHits.add(hit2);
                                    }
                                    if (cursor != null) {
                                        cursor.close();
                                    }
                                    return partialHits;
                                } catch (Throwable th2) {
                                    if (cursor != null) {
                                        cursor.close();
                                    }
                                }
                            }
                        } else {
                            ((Hit) hits2.get(count)).setHitString(cursor.getString(1));
                        }
                        count++;
                    } while (cursor.moveToNext());
                }
                if (cursor == null) {
                    return hits2;
                }
                cursor.close();
                return hits2;
            } catch (SQLiteException e3) {
                e2 = e3;
                hits = hits2;
                try {
                    Log.w("error in peekHits fetching hitIds: " + e2.getMessage());
                    hits2 = new ArrayList();
                    if (cursor != null) {
                        return hits2;
                    }
                    cursor.close();
                    return hits2;
                } catch (Throwable th3) {
                    th = th3;
                    if (cursor != null) {
                        cursor.close();
                    }
                    throw th;
                }
            } catch (Throwable th4) {
                th = th4;
                hits = hits2;
                if (cursor != null) {
                    cursor.close();
                }
                throw th;
            }
        } catch (SQLiteException e4) {
            e2 = e4;
            Log.w("error in peekHits fetching hitIds: " + e2.getMessage());
            hits2 = new ArrayList();
            if (cursor != null) {
                return hits2;
            }
            cursor.close();
            return hits2;
        }
    }

    @VisibleForTesting
    void setLastDeleteStaleHitsTime(long timeInMilliseconds) {
        this.mLastDeleteStaleHitsTime = timeInMilliseconds;
    }

    int deleteStaleHits() {
        boolean z = true;
        long now = this.mClock.currentTimeMillis();
        if (now <= this.mLastDeleteStaleHitsTime + 86400000) {
            return 0;
        }
        this.mLastDeleteStaleHitsTime = now;
        SQLiteDatabase db = getWritableDatabase("Error opening database for deleteStaleHits");
        if (db == null) {
            return 0;
        }
        long lastGoodTime = this.mClock.currentTimeMillis() - 2592000000L;
        int rslt = db.delete(HITS_TABLE, "HIT_TIME < ?", new String[]{Long.toString(lastGoodTime)});
        AnalyticsStoreStateListener analyticsStoreStateListener = this.mListener;
        if (getNumStoredHits() != 0) {
            z = false;
        }
        analyticsStoreStateListener.reportStoreIsEmpty(z);
        return rslt;
    }

    public void deleteHits(Collection<Hit> hits) {
        if (hits == null) {
            throw new NullPointerException("hits cannot be null");
        } else if (!hits.isEmpty()) {
            SQLiteDatabase db = getWritableDatabase("Error opening database for deleteHit");
            if (db != null) {
                String[] ids = new String[hits.size()];
                String whereClause = String.format("HIT_ID in (%s)", new Object[]{TextUtils.join(",", Collections.nCopies(ids.length, "?"))});
                int i = 0;
                for (Hit hit : hits) {
                    int i2 = i + 1;
                    ids[i] = Long.toString(hit.getHitId());
                    i = i2;
                }
                try {
                    db.delete(HITS_TABLE, whereClause, ids);
                    this.mListener.reportStoreIsEmpty(getNumStoredHits() == 0);
                } catch (SQLiteException e) {
                    Log.w("Error deleting hit " + hits);
                }
            }
        }
    }

    int getNumStoredHits() {
        int numStoredHits = 0;
        SQLiteDatabase db = getWritableDatabase("Error opening database for requestNumHitsPending");
        if (db == null) {
            return 0;
        }
        Cursor cursor = null;
        try {
            cursor = db.rawQuery("SELECT COUNT(*) from hits2", null);
            if (cursor.moveToFirst()) {
                numStoredHits = (int) cursor.getLong(0);
            }
            if (cursor != null) {
                cursor.close();
            }
        } catch (SQLiteException e) {
            Log.w("Error getting numStoredHits");
            if (cursor != null) {
                cursor.close();
            }
        } catch (Throwable th) {
            if (cursor != null) {
                cursor.close();
            }
        }
        return numStoredHits;
    }

    public void dispatch() {
        Log.vDebug("dispatch running...");
        if (this.mDispatcher.okToDispatch()) {
            List<Hit> hits = peekHits(40);
            if (hits.isEmpty()) {
                Log.vDebug("...nothing to dispatch");
                this.mListener.reportStoreIsEmpty(true);
                return;
            }
            int hitsDispatched = this.mDispatcher.dispatchHits(hits);
            Log.vDebug("sent " + hitsDispatched + " of " + hits.size() + " hits");
            deleteHits(hits.subList(0, Math.min(hitsDispatched, hits.size())));
            if (hitsDispatched == hits.size() && getNumStoredHits() > 0) {
                GAServiceManager.getInstance().dispatch();
            }
        }
    }

    public void close() {
        try {
            this.mDbHelper.getWritableDatabase().close();
        } catch (SQLiteException e) {
            Log.w("Error opening database for close");
        }
    }

    @VisibleForTesting
    AnalyticsDatabaseHelper getHelper() {
        return this.mDbHelper;
    }

    private SQLiteDatabase getWritableDatabase(String errorMessage) {
        try {
            return this.mDbHelper.getWritableDatabase();
        } catch (SQLiteException e) {
            Log.w(errorMessage);
            return null;
        }
    }
}
