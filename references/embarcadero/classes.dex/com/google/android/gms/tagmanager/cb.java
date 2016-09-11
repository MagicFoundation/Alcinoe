package com.google.android.gms.tagmanager;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteCursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteException;
import android.database.sqlite.SQLiteOpenHelper;
import android.os.Build.VERSION;
import android.text.TextUtils;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.internal.fl;
import com.google.android.gms.internal.fn;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.apache.http.impl.client.DefaultHttpClient;

class cb implements at {
    private static final String ua;
    private fl Ty;
    private final b VL;
    private volatile ab VM;
    private final au VN;
    private final Context mContext;
    private final String ud;
    private long uf;
    private final int ug;

    class b extends SQLiteOpenHelper {
        final /* synthetic */ cb VO;
        private boolean ui;
        private long uj;

        b(cb cbVar, Context context, String str) {
            this.VO = cbVar;
            super(context, str, null, 1);
            this.uj = 0;
        }

        private void a(SQLiteDatabase sQLiteDatabase) {
            Cursor rawQuery = sQLiteDatabase.rawQuery("SELECT * FROM gtm_hits WHERE 0", null);
            Set hashSet = new HashSet();
            try {
                String[] columnNames = rawQuery.getColumnNames();
                for (Object add : columnNames) {
                    hashSet.add(add);
                }
                if (!hashSet.remove("hit_id") || !hashSet.remove("hit_url") || !hashSet.remove("hit_time") || !hashSet.remove("hit_first_send_time")) {
                    throw new SQLiteException("Database column missing");
                } else if (!hashSet.isEmpty()) {
                    throw new SQLiteException("Database has extra columns");
                }
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
                        bh.w("Error querying for table " + str);
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
                bh.w("Error querying for table " + str);
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
            if (!this.ui || this.uj + 3600000 <= this.VO.Ty.currentTimeMillis()) {
                SQLiteDatabase sQLiteDatabase = null;
                this.ui = true;
                this.uj = this.VO.Ty.currentTimeMillis();
                try {
                    sQLiteDatabase = super.getWritableDatabase();
                } catch (SQLiteException e) {
                    this.VO.mContext.getDatabasePath(this.VO.ud).delete();
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
            ak.B(db.getPath());
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
            if (a("gtm_hits", db)) {
                a(db);
            } else {
                db.execSQL(cb.ua);
            }
        }

        public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        }
    }

    class a implements com.google.android.gms.tagmanager.db.a {
        final /* synthetic */ cb VO;

        a(cb cbVar) {
            this.VO = cbVar;
        }

        public void a(ap apVar) {
            this.VO.u(apVar.ci());
        }

        public void b(ap apVar) {
            this.VO.u(apVar.ci());
            bh.v("Permanent failure dispatching hitId: " + apVar.ci());
        }

        public void c(ap apVar) {
            long je = apVar.je();
            if (je == 0) {
                this.VO.c(apVar.ci(), this.VO.Ty.currentTimeMillis());
            } else if (je + 14400000 < this.VO.Ty.currentTimeMillis()) {
                this.VO.u(apVar.ci());
                bh.v("Giving up on failed hitId: " + apVar.ci());
            }
        }
    }

    static {
        ua = String.format("CREATE TABLE IF NOT EXISTS %s ( '%s' INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, '%s' INTEGER NOT NULL, '%s' TEXT NOT NULL,'%s' INTEGER NOT NULL);", new Object[]{"gtm_hits", "hit_id", "hit_time", "hit_url", "hit_first_send_time"});
    }

    cb(au auVar, Context context) {
        this(auVar, context, "gtm_urls.db", GamesStatusCodes.STATUS_REQUEST_UPDATE_PARTIAL_SUCCESS);
    }

    cb(au auVar, Context context, String str, int i) {
        this.mContext = context.getApplicationContext();
        this.ud = str;
        this.VN = auVar;
        this.Ty = fn.eI();
        this.VL = new b(this, this.mContext, this.ud);
        this.VM = new db(new DefaultHttpClient(), this.mContext, new a(this));
        this.uf = 0;
        this.ug = i;
    }

    private SQLiteDatabase G(String str) {
        try {
            return this.VL.getWritableDatabase();
        } catch (SQLiteException e) {
            bh.w(str);
            return null;
        }
    }

    private void c(long j, long j2) {
        SQLiteDatabase G = G("Error opening database for getNumStoredHits.");
        if (G != null) {
            ContentValues contentValues = new ContentValues();
            contentValues.put("hit_first_send_time", Long.valueOf(j2));
            try {
                G.update("gtm_hits", contentValues, "hit_id=?", new String[]{String.valueOf(j)});
            } catch (SQLiteException e) {
                bh.w("Error setting HIT_FIRST_DISPATCH_TIME for hitId: " + j);
                u(j);
            }
        }
    }

    private void co() {
        int cq = (cq() - this.ug) + 1;
        if (cq > 0) {
            List s = s(cq);
            bh.v("Store full, deleting " + s.size() + " hits to make room.");
            a((String[]) s.toArray(new String[0]));
        }
    }

    private void f(long j, String str) {
        SQLiteDatabase G = G("Error opening database for putHit");
        if (G != null) {
            ContentValues contentValues = new ContentValues();
            contentValues.put("hit_time", Long.valueOf(j));
            contentValues.put("hit_url", str);
            contentValues.put("hit_first_send_time", Integer.valueOf(0));
            try {
                G.insert("gtm_hits", null, contentValues);
                this.VN.p(false);
            } catch (SQLiteException e) {
                bh.w("Error storing hit");
            }
        }
    }

    private void u(long j) {
        a(new String[]{String.valueOf(j)});
    }

    void a(String[] strArr) {
        boolean z = true;
        if (strArr != null && strArr.length != 0) {
            SQLiteDatabase G = G("Error opening database for deleteHits.");
            if (G != null) {
                try {
                    G.delete("gtm_hits", String.format("HIT_ID in (%s)", new Object[]{TextUtils.join(",", Collections.nCopies(strArr.length, "?"))}), strArr);
                    au auVar = this.VN;
                    if (cq() != 0) {
                        z = false;
                    }
                    auVar.p(z);
                } catch (SQLiteException e) {
                    bh.w("Error deleting hits");
                }
            }
        }
    }

    public void bp() {
        bh.v("GTM Dispatch running...");
        if (this.VM.bA()) {
            List t = t(40);
            if (t.isEmpty()) {
                bh.v("...nothing to dispatch");
                this.VN.p(true);
                return;
            }
            this.VM.e(t);
            if (js() > 0) {
                cy.kh().bp();
            }
        }
    }

    int cp() {
        boolean z = true;
        long currentTimeMillis = this.Ty.currentTimeMillis();
        if (currentTimeMillis <= this.uf + 86400000) {
            return 0;
        }
        this.uf = currentTimeMillis;
        SQLiteDatabase G = G("Error opening database for deleteStaleHits.");
        if (G == null) {
            return 0;
        }
        int delete = G.delete("gtm_hits", "HIT_TIME < ?", new String[]{Long.toString(this.Ty.currentTimeMillis() - 2592000000L)});
        au auVar = this.VN;
        if (cq() != 0) {
            z = false;
        }
        auVar.p(z);
        return delete;
    }

    int cq() {
        Cursor cursor = null;
        int i = 0;
        SQLiteDatabase G = G("Error opening database for getNumStoredHits.");
        if (G != null) {
            try {
                cursor = G.rawQuery("SELECT COUNT(*) from gtm_hits", null);
                if (cursor.moveToFirst()) {
                    i = (int) cursor.getLong(0);
                }
                if (cursor != null) {
                    cursor.close();
                }
            } catch (SQLiteException e) {
                bh.w("Error getting numStoredHits");
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

    public void e(long j, String str) {
        cp();
        co();
        f(j, str);
    }

    int js() {
        Cursor cursor;
        Throwable th;
        Cursor cursor2 = null;
        SQLiteDatabase G = G("Error opening database for getNumStoredHits.");
        if (G == null) {
            return 0;
        }
        int count;
        try {
            Cursor query = G.query("gtm_hits", new String[]{"hit_id", "hit_first_send_time"}, "hit_first_send_time=0", null, null, null, null);
            try {
                count = query.getCount();
                if (query != null) {
                    query.close();
                }
            } catch (SQLiteException e) {
                cursor = query;
                try {
                    bh.w("Error getting num untried hits");
                    if (cursor == null) {
                        count = 0;
                    } else {
                        cursor.close();
                        count = 0;
                    }
                    return count;
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
            bh.w("Error getting num untried hits");
            if (cursor == null) {
                cursor.close();
                count = 0;
            } else {
                count = 0;
            }
            return count;
        } catch (Throwable th4) {
            th = th4;
            if (cursor2 != null) {
                cursor2.close();
            }
            throw th;
        }
        return count;
    }

    List<String> s(int i) {
        Cursor query;
        SQLiteException e;
        Throwable th;
        List<String> arrayList = new ArrayList();
        if (i <= 0) {
            bh.w("Invalid maxHits specified. Skipping");
            return arrayList;
        }
        SQLiteDatabase G = G("Error opening database for peekHitIds.");
        if (G == null) {
            return arrayList;
        }
        try {
            query = G.query("gtm_hits", new String[]{"hit_id"}, null, null, null, null, String.format("%s ASC", new Object[]{"hit_id"}), Integer.toString(i));
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
                    bh.w("Error in peekHits fetching hitIds: " + e.getMessage());
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
            bh.w("Error in peekHits fetching hitIds: " + e.getMessage());
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
    public List<ap> t(int i) {
        List<ap> arrayList;
        SQLiteException e;
        Throwable th;
        SQLiteException sQLiteException;
        Cursor cursor;
        List<ap> list;
        ArrayList arrayList2 = new ArrayList();
        SQLiteDatabase G = G("Error opening database for peekHits");
        if (G == null) {
            return arrayList2;
        }
        Cursor cursor2 = null;
        try {
            Cursor query = G.query("gtm_hits", new String[]{"hit_id", "hit_time", "hit_first_send_time"}, null, null, null, null, String.format("%s ASC", new Object[]{"hit_id"}), Integer.toString(i));
            try {
                arrayList = new ArrayList();
                if (query.moveToFirst()) {
                    do {
                        arrayList.add(new ap(query.getLong(0), query.getLong(1), query.getLong(2)));
                    } while (query.moveToNext());
                }
                if (query != null) {
                    query.close();
                }
                try {
                    Cursor query2 = G.query("gtm_hits", new String[]{"hit_id", "hit_url"}, null, null, null, null, String.format("%s ASC", new Object[]{"hit_id"}), Integer.toString(i));
                    try {
                        if (query2.moveToFirst()) {
                            int i2 = 0;
                            while (true) {
                                if (((SQLiteCursor) query2).getWindow().getNumRows() > 0) {
                                    ((ap) arrayList.get(i2)).F(query2.getString(1));
                                } else {
                                    bh.w(String.format("HitString for hitId %d too large.  Hit will be deleted.", new Object[]{Long.valueOf(((ap) arrayList.get(i2)).ci())}));
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
                        return arrayList;
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
                        bh.w("Error in peekHits fetching hit url: " + e.getMessage());
                        List<ap> arrayList3 = new ArrayList();
                        Object obj = null;
                        for (ap apVar : arrayList) {
                            if (TextUtils.isEmpty(apVar.jf())) {
                                if (obj != null) {
                                    break;
                                }
                                obj = 1;
                            }
                            arrayList3.add(apVar);
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
                list = arrayList;
                try {
                    bh.w("Error in peekHits fetching hitIds: " + sQLiteException.getMessage());
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
            list = arrayList2;
            bh.w("Error in peekHits fetching hitIds: " + sQLiteException.getMessage());
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
