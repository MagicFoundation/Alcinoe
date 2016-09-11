package com.google.android.gms.analytics;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteCursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteException;
import android.database.sqlite.SQLiteOpenHelper;
import android.os.Build.VERSION;
import android.text.TextUtils;
import com.google.analytics.tracking.android.ModelFields;
import com.google.android.gms.analytics.internal.Command;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.internal.di;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.http.impl.client.DefaultHttpClient;

class ac implements d {
    private static final String ua;
    private final Context mContext;
    private i rJ;
    private final e rr;
    private final a ub;
    private volatile n uc;
    private final String ud;
    private ab ue;
    private long uf;
    private final int ug;

    class a extends SQLiteOpenHelper {
        final /* synthetic */ ac uh;
        private boolean ui;
        private long uj;

        a(ac acVar, Context context, String str) {
            this.uh = acVar;
            super(context, str, null, 1);
            this.uj = 0;
        }

        private void a(SQLiteDatabase sQLiteDatabase) {
            Object obj = null;
            Cursor rawQuery = sQLiteDatabase.rawQuery("SELECT * FROM hits2 WHERE 0", null);
            Set hashSet = new HashSet();
            try {
                String[] columnNames = rawQuery.getColumnNames();
                for (Object add : columnNames) {
                    hashSet.add(add);
                }
                if (hashSet.remove("hit_id") && hashSet.remove("hit_url") && hashSet.remove("hit_string") && hashSet.remove("hit_time")) {
                    if (!hashSet.remove("hit_app_id")) {
                        obj = 1;
                    }
                    if (!hashSet.isEmpty()) {
                        throw new SQLiteException("Database has extra columns");
                    } else if (obj != null) {
                        sQLiteDatabase.execSQL("ALTER TABLE hits2 ADD COLUMN hit_app_id");
                        return;
                    } else {
                        return;
                    }
                }
                throw new SQLiteException("Database column missing");
            } finally {
                rawQuery.close();
            }
        }

        private boolean a(String str, SQLiteDatabase sQLiteDatabase) {
            Cursor cursor;
            Throwable th;
            Cursor cursor2 = null;
            try {
                SQLiteDatabase sQLiteDatabase2 = sQLiteDatabase;
                Cursor query = sQLiteDatabase2.query("SQLITE_MASTER", new String[]{"name"}, "name=?", new String[]{str}, null, null, null);
                try {
                    boolean moveToFirst = query.moveToFirst();
                    if (query == null) {
                        return moveToFirst;
                    }
                    query.close();
                    return moveToFirst;
                } catch (SQLiteException e) {
                    cursor = query;
                    try {
                        aa.w("Error querying for table " + str);
                        if (cursor != null) {
                            cursor.close();
                        }
                        return false;
                    } catch (Throwable th2) {
                        cursor2 = cursor;
                        th = th2;
                        if (cursor2 != null) {
                            cursor2.close();
                        }
                        throw th;
                    }
                } catch (Throwable th3) {
                    th = th3;
                    cursor2 = query;
                    if (cursor2 != null) {
                        cursor2.close();
                    }
                    throw th;
                }
            } catch (SQLiteException e2) {
                cursor = null;
                aa.w("Error querying for table " + str);
                if (cursor != null) {
                    cursor.close();
                }
                return false;
            } catch (Throwable th4) {
                th = th4;
                if (cursor2 != null) {
                    cursor2.close();
                }
                throw th;
            }
        }

        public SQLiteDatabase getWritableDatabase() {
            if (!this.ui || this.uj + 3600000 <= this.uh.rJ.currentTimeMillis()) {
                SQLiteDatabase sQLiteDatabase = null;
                this.ui = true;
                this.uj = this.uh.rJ.currentTimeMillis();
                try {
                    sQLiteDatabase = super.getWritableDatabase();
                } catch (SQLiteException e) {
                    this.uh.mContext.getDatabasePath(this.uh.ud).delete();
                }
                if (sQLiteDatabase == null) {
                    sQLiteDatabase = super.getWritableDatabase();
                }
                this.ui = false;
                return sQLiteDatabase;
            }
            throw new SQLiteException("Database creation failed");
        }

        public void onCreate(SQLiteDatabase db) {
            p.B(db.getPath());
        }

        public void onOpen(SQLiteDatabase db) {
            if (VERSION.SDK_INT < 15) {
                Cursor rawQuery = db.rawQuery("PRAGMA journal_mode=memory", null);
                try {
                    rawQuery.moveToFirst();
                } finally {
                    rawQuery.close();
                }
            }
            if (a("hits2", db)) {
                a(db);
            } else {
                db.execSQL(ac.ua);
            }
        }

        public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        }
    }

    static {
        ua = String.format("CREATE TABLE IF NOT EXISTS %s ( '%s' INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, '%s' INTEGER NOT NULL, '%s' TEXT NOT NULL, '%s' TEXT NOT NULL, '%s' INTEGER);", new Object[]{"hits2", "hit_id", "hit_time", "hit_url", "hit_string", "hit_app_id"});
    }

    ac(e eVar, Context context) {
        this(eVar, context, "google_analytics_v4.db", GamesStatusCodes.STATUS_REQUEST_UPDATE_PARTIAL_SUCCESS);
    }

    ac(e eVar, Context context, String str, int i) {
        this.mContext = context.getApplicationContext();
        this.ud = str;
        this.rr = eVar;
        this.rJ = new i() {
            final /* synthetic */ ac uh;

            {
                this.uh = r1;
            }

            public long currentTimeMillis() {
                return System.currentTimeMillis();
            }
        };
        this.ub = new a(this, this.mContext, this.ud);
        this.uc = new ah(new DefaultHttpClient(), this.mContext);
        this.uf = 0;
        this.ug = i;
    }

    private SQLiteDatabase G(String str) {
        try {
            return this.ub.getWritableDatabase();
        } catch (SQLiteException e) {
            aa.w(str);
            return null;
        }
    }

    private void a(Map<String, String> map, long j, String str) {
        SQLiteDatabase G = G("Error opening database for putHit");
        if (G != null) {
            long parseLong;
            ContentValues contentValues = new ContentValues();
            contentValues.put("hit_string", t((Map) map));
            contentValues.put("hit_time", Long.valueOf(j));
            if (map.containsKey(ModelFields.ANDROID_APP_UID)) {
                try {
                    parseLong = Long.parseLong((String) map.get(ModelFields.ANDROID_APP_UID));
                } catch (NumberFormatException e) {
                    parseLong = 0;
                }
            } else {
                parseLong = 0;
            }
            contentValues.put("hit_app_id", Long.valueOf(parseLong));
            if (str == null) {
                str = "http://www.google-analytics.com/collect";
            }
            if (str.length() == 0) {
                aa.w("Empty path: not sending hit");
                return;
            }
            contentValues.put("hit_url", str);
            try {
                G.insert("hits2", null, contentValues);
                this.rr.p(false);
            } catch (SQLiteException e2) {
                aa.w("Error storing hit");
            }
        }
    }

    private void a(Map<String, String> map, Collection<di> collection) {
        String substring = "&_v".substring(1);
        if (collection != null) {
            for (di diVar : collection) {
                if (Command.APPEND_VERSION.equals(diVar.getId())) {
                    map.put(substring, diVar.getValue());
                    return;
                }
            }
        }
    }

    private void co() {
        int cq = (cq() - this.ug) + 1;
        if (cq > 0) {
            List s = s(cq);
            aa.v("Store full, deleting " + s.size() + " hits to make room.");
            a((String[]) s.toArray(new String[0]));
        }
    }

    static String t(Map<String, String> map) {
        Iterable arrayList = new ArrayList(map.size());
        for (Entry entry : map.entrySet()) {
            arrayList.add(y.encode((String) entry.getKey()) + "=" + y.encode((String) entry.getValue()));
        }
        return TextUtils.join("&", arrayList);
    }

    public void a(Map<String, String> map, long j, String str, Collection<di> collection) {
        cp();
        co();
        a(map, collection);
        a(map, j, str);
    }

    void a(String[] strArr) {
        boolean z = true;
        if (strArr == null || strArr.length == 0) {
            aa.w("Empty hitIds passed to deleteHits.");
            return;
        }
        SQLiteDatabase G = G("Error opening database for deleteHits.");
        if (G != null) {
            try {
                G.delete("hits2", String.format("HIT_ID in (%s)", new Object[]{TextUtils.join(",", Collections.nCopies(strArr.length, "?"))}), strArr);
                e eVar = this.rr;
                if (cq() != 0) {
                    z = false;
                }
                eVar.p(z);
            } catch (SQLiteException e) {
                aa.w("Error deleting hits " + strArr);
            }
        }
    }

    @Deprecated
    void b(Collection<x> collection) {
        if (collection == null || collection.isEmpty()) {
            aa.w("Empty/Null collection passed to deleteHits.");
            return;
        }
        String[] strArr = new String[collection.size()];
        int i = 0;
        for (x ci : collection) {
            int i2 = i + 1;
            strArr[i] = String.valueOf(ci.ci());
            i = i2;
        }
        a(strArr);
    }

    public void bp() {
        boolean z = true;
        aa.v("Dispatch running...");
        if (this.uc.bA()) {
            List t = t(40);
            if (t.isEmpty()) {
                aa.v("...nothing to dispatch");
                this.rr.p(true);
                return;
            }
            if (this.ue == null) {
                this.ue = new ab("_t=dispatch&_v=ma4.0.0", true);
            }
            if (cq() > t.size()) {
                z = false;
            }
            int a = this.uc.a(t, this.ue, z);
            aa.v("sent " + a + " of " + t.size() + " hits");
            b(t.subList(0, Math.min(a, t.size())));
            if (a != t.size() || cq() <= 0) {
                this.ue = null;
            } else {
                GoogleAnalytics.getInstance(this.mContext).dispatchLocalHits();
            }
        }
    }

    public n bq() {
        return this.uc;
    }

    int cp() {
        boolean z = true;
        long currentTimeMillis = this.rJ.currentTimeMillis();
        if (currentTimeMillis <= this.uf + 86400000) {
            return 0;
        }
        this.uf = currentTimeMillis;
        SQLiteDatabase G = G("Error opening database for deleteStaleHits.");
        if (G == null) {
            return 0;
        }
        int delete = G.delete("hits2", "HIT_TIME < ?", new String[]{Long.toString(this.rJ.currentTimeMillis() - 2592000000L)});
        e eVar = this.rr;
        if (cq() != 0) {
            z = false;
        }
        eVar.p(z);
        return delete;
    }

    int cq() {
        Cursor cursor = null;
        int i = 0;
        SQLiteDatabase G = G("Error opening database for getNumStoredHits.");
        if (G != null) {
            try {
                cursor = G.rawQuery("SELECT COUNT(*) from hits2", null);
                if (cursor.moveToFirst()) {
                    i = (int) cursor.getLong(0);
                }
                if (cursor != null) {
                    cursor.close();
                }
            } catch (SQLiteException e) {
                aa.w("Error getting numStoredHits");
                if (cursor != null) {
                    cursor.close();
                }
            } catch (Throwable th) {
                if (cursor != null) {
                    cursor.close();
                }
            }
        }
        return i;
    }

    public void i(long j) {
        boolean z = true;
        SQLiteDatabase G = G("Error opening database for clearHits");
        if (G != null) {
            if (j == 0) {
                G.delete("hits2", null, null);
            } else {
                G.delete("hits2", "hit_app_id = ?", new String[]{Long.valueOf(j).toString()});
            }
            e eVar = this.rr;
            if (cq() != 0) {
                z = false;
            }
            eVar.p(z);
        }
    }

    List<String> s(int i) {
        Cursor query;
        SQLiteException e;
        Throwable th;
        List<String> arrayList = new ArrayList();
        if (i <= 0) {
            aa.w("Invalid maxHits specified. Skipping");
            return arrayList;
        }
        SQLiteDatabase G = G("Error opening database for peekHitIds.");
        if (G == null) {
            return arrayList;
        }
        try {
            query = G.query("hits2", new String[]{"hit_id"}, null, null, null, null, String.format("%s ASC", new Object[]{"hit_id"}), Integer.toString(i));
            try {
                if (query.moveToFirst()) {
                    do {
                        arrayList.add(String.valueOf(query.getLong(0)));
                    } while (query.moveToNext());
                }
                if (query != null) {
                    query.close();
                }
            } catch (SQLiteException e2) {
                e = e2;
                try {
                    aa.w("Error in peekHits fetching hitIds: " + e.getMessage());
                    if (query != null) {
                        query.close();
                    }
                    return arrayList;
                } catch (Throwable th2) {
                    th = th2;
                    if (query != null) {
                        query.close();
                    }
                    throw th;
                }
            }
        } catch (SQLiteException e3) {
            e = e3;
            query = null;
            aa.w("Error in peekHits fetching hitIds: " + e.getMessage());
            if (query != null) {
                query.close();
            }
            return arrayList;
        } catch (Throwable th3) {
            th = th3;
            query = null;
            if (query != null) {
                query.close();
            }
            throw th;
        }
        return arrayList;
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    public List<x> t(int i) {
        SQLiteException e;
        Throwable th;
        SQLiteException sQLiteException;
        Cursor cursor;
        List<x> list;
        ArrayList arrayList = new ArrayList();
        SQLiteDatabase G = G("Error opening database for peekHits");
        if (G == null) {
            return arrayList;
        }
        Cursor cursor2 = null;
        try {
            Cursor query = G.query("hits2", new String[]{"hit_id", "hit_time"}, null, null, null, null, String.format("%s ASC", new Object[]{"hit_id"}), Integer.toString(i));
            List<x> arrayList2;
            try {
                arrayList2 = new ArrayList();
                if (query.moveToFirst()) {
                    do {
                        arrayList2.add(new x(null, query.getLong(0), query.getLong(1)));
                    } while (query.moveToNext());
                }
                if (query != null) {
                    query.close();
                }
                try {
                    Cursor query2 = G.query("hits2", new String[]{"hit_id", "hit_string", "hit_url"}, null, null, null, null, String.format("%s ASC", new Object[]{"hit_id"}), Integer.toString(i));
                    try {
                        if (query2.moveToFirst()) {
                            int i2 = 0;
                            while (true) {
                                if (((SQLiteCursor) query2).getWindow().getNumRows() > 0) {
                                    ((x) arrayList2.get(i2)).E(query2.getString(1));
                                    ((x) arrayList2.get(i2)).F(query2.getString(2));
                                } else {
                                    aa.w(String.format("HitString for hitId %d too large.  Hit will be deleted.", new Object[]{Long.valueOf(((x) arrayList2.get(i2)).ci())}));
                                }
                                int i3 = i2 + 1;
                                if (!query2.moveToNext()) {
                                    break;
                                }
                                i2 = i3;
                            }
                        }
                        if (query2 != null) {
                            query2.close();
                        }
                        return arrayList2;
                    } catch (SQLiteException e2) {
                        e = e2;
                        query = query2;
                    } catch (Throwable th2) {
                        th = th2;
                        query = query2;
                    }
                } catch (SQLiteException e3) {
                    e = e3;
                    try {
                        aa.w("Error in peekHits fetching hitString: " + e.getMessage());
                        List<x> arrayList3 = new ArrayList();
                        Object obj = null;
                        for (x xVar : arrayList2) {
                            if (TextUtils.isEmpty(xVar.ch())) {
                                if (obj != null) {
                                    break;
                                }
                                obj = 1;
                            }
                            arrayList3.add(xVar);
                        }
                        if (query != null) {
                            query.close();
                        }
                        return arrayList3;
                    } catch (Throwable th3) {
                        th = th3;
                    }
                }
            } catch (SQLiteException e4) {
                sQLiteException = e4;
                cursor = query;
                list = arrayList2;
                try {
                    aa.w("Error in peekHits fetching hitIds: " + sQLiteException.getMessage());
                    if (cursor == null) {
                        return list;
                    }
                    cursor.close();
                    return list;
                } catch (Throwable th4) {
                    th = th4;
                    cursor2 = cursor;
                    if (cursor2 != null) {
                        cursor2.close();
                    }
                    throw th;
                }
            } catch (Throwable th5) {
                th = th5;
                cursor2 = query;
                if (cursor2 != null) {
                    cursor2.close();
                }
                throw th;
            }
        } catch (SQLiteException e42) {
            sQLiteException = e42;
            cursor = null;
            list = arrayList;
            aa.w("Error in peekHits fetching hitIds: " + sQLiteException.getMessage());
            if (cursor == null) {
                return list;
            }
            cursor.close();
            return list;
        } catch (Throwable th6) {
            th = th6;
            if (cursor2 != null) {
                cursor2.close();
            }
            throw th;
        }
    }
}
