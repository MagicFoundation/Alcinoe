package com.google.android.gms.tagmanager;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteException;
import android.database.sqlite.SQLiteOpenHelper;
import android.os.Build.VERSION;
import android.text.TextUtils;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.internal.fl;
import com.google.android.gms.internal.fn;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

class v implements c {
    private static final String UD;
    private fl Ty;
    private final Executor UE;
    private a UF;
    private int UG;
    private final Context mContext;

    /* renamed from: com.google.android.gms.tagmanager.v.1 */
    class AnonymousClass1 implements Runnable {
        final /* synthetic */ List UH;
        final /* synthetic */ long UI;
        final /* synthetic */ v UJ;

        AnonymousClass1(v vVar, List list, long j) {
            this.UJ = vVar;
            this.UH = list;
            this.UI = j;
        }

        public void run() {
            this.UJ.b(this.UH, this.UI);
        }
    }

    /* renamed from: com.google.android.gms.tagmanager.v.2 */
    class AnonymousClass2 implements Runnable {
        final /* synthetic */ v UJ;
        final /* synthetic */ com.google.android.gms.tagmanager.DataLayer.c.a UK;

        AnonymousClass2(v vVar, com.google.android.gms.tagmanager.DataLayer.c.a aVar) {
            this.UJ = vVar;
            this.UK = aVar;
        }

        public void run() {
            this.UK.b(this.UJ.iT());
        }
    }

    /* renamed from: com.google.android.gms.tagmanager.v.3 */
    class AnonymousClass3 implements Runnable {
        final /* synthetic */ v UJ;
        final /* synthetic */ String UL;

        AnonymousClass3(v vVar, String str) {
            this.UJ = vVar;
            this.UL = str;
        }

        public void run() {
            this.UJ.bj(this.UL);
        }
    }

    class a extends SQLiteOpenHelper {
        final /* synthetic */ v UJ;

        a(v vVar, Context context, String str) {
            this.UJ = vVar;
            super(context, str, null, 1);
        }

        private void a(SQLiteDatabase sQLiteDatabase) {
            Cursor rawQuery = sQLiteDatabase.rawQuery("SELECT * FROM datalayer WHERE 0", null);
            Set hashSet = new HashSet();
            try {
                String[] columnNames = rawQuery.getColumnNames();
                for (Object add : columnNames) {
                    hashSet.add(add);
                }
                if (!hashSet.remove("key") || !hashSet.remove("value") || !hashSet.remove("ID") || !hashSet.remove("expires")) {
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
            SQLiteDatabase sQLiteDatabase = null;
            try {
                sQLiteDatabase = super.getWritableDatabase();
            } catch (SQLiteException e) {
                this.UJ.mContext.getDatabasePath("google_tagmanager.db").delete();
            }
            return sQLiteDatabase == null ? super.getWritableDatabase() : sQLiteDatabase;
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
            if (a("datalayer", db)) {
                a(db);
            } else {
                db.execSQL(v.UD);
            }
        }

        public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        }
    }

    private static class b {
        final String UA;
        final byte[] UM;

        b(String str, byte[] bArr) {
            this.UA = str;
            this.UM = bArr;
        }

        public String toString() {
            return "KeyAndSerialized: key = " + this.UA + " serialized hash = " + Arrays.hashCode(this.UM);
        }
    }

    static {
        UD = String.format("CREATE TABLE IF NOT EXISTS %s ( '%s' INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, '%s' STRING NOT NULL, '%s' BLOB NOT NULL, '%s' INTEGER NOT NULL);", new Object[]{"datalayer", "ID", "key", "value", "expires"});
    }

    public v(Context context) {
        this(context, fn.eI(), "google_tagmanager.db", GamesStatusCodes.STATUS_REQUEST_UPDATE_PARTIAL_SUCCESS, Executors.newSingleThreadExecutor());
    }

    v(Context context, fl flVar, String str, int i, Executor executor) {
        this.mContext = context;
        this.Ty = flVar;
        this.UG = i;
        this.UE = executor;
        this.UF = new a(this, this.mContext, str);
    }

    private SQLiteDatabase G(String str) {
        try {
            return this.UF.getWritableDatabase();
        } catch (SQLiteException e) {
            bh.w(str);
            return null;
        }
    }

    private synchronized void b(List<b> list, long j) {
        try {
            long currentTimeMillis = this.Ty.currentTimeMillis();
            t(currentTimeMillis);
            bQ(list.size());
            c(list, currentTimeMillis + j);
            iW();
        } catch (Throwable th) {
            iW();
        }
    }

    private void bQ(int i) {
        int iV = (iV() - this.UG) + i;
        if (iV > 0) {
            List bR = bR(iV);
            bh.u("DataLayer store full, deleting " + bR.size() + " entries to make room.");
            g((String[]) bR.toArray(new String[0]));
        }
    }

    private List<String> bR(int i) {
        SQLiteException e;
        Throwable th;
        List<String> arrayList = new ArrayList();
        if (i <= 0) {
            bh.w("Invalid maxEntries specified. Skipping.");
            return arrayList;
        }
        SQLiteDatabase G = G("Error opening database for peekEntryIds.");
        if (G == null) {
            return arrayList;
        }
        Cursor query;
        try {
            query = G.query("datalayer", new String[]{"ID"}, null, null, null, null, String.format("%s ASC", new Object[]{"ID"}), Integer.toString(i));
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
                    bh.w("Error in peekEntries fetching entryIds: " + e.getMessage());
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
            bh.w("Error in peekEntries fetching entryIds: " + e.getMessage());
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

    private void bj(String str) {
        SQLiteDatabase G = G("Error opening database for clearKeysWithPrefix.");
        if (G != null) {
            try {
                bh.v("Cleared " + G.delete("datalayer", "key = ? OR key LIKE ?", new String[]{str, str + ".%"}) + " items");
            } catch (SQLiteException e) {
                bh.w("Error deleting entries with key prefix: " + str + " (" + e + ").");
            } finally {
                iW();
            }
        }
    }

    private List<a> c(List<b> list) {
        List<a> arrayList = new ArrayList();
        for (b bVar : list) {
            arrayList.add(new a(bVar.UA, j(bVar.UM)));
        }
        return arrayList;
    }

    private void c(List<b> list, long j) {
        SQLiteDatabase G = G("Error opening database for writeEntryToDatabase.");
        if (G != null) {
            for (b bVar : list) {
                ContentValues contentValues = new ContentValues();
                contentValues.put("expires", Long.valueOf(j));
                contentValues.put("key", bVar.UA);
                contentValues.put("value", bVar.UM);
                G.insert("datalayer", null, contentValues);
            }
        }
    }

    private List<b> d(List<a> list) {
        List<b> arrayList = new ArrayList();
        for (a aVar : list) {
            arrayList.add(new b(aVar.UA, j(aVar.UB)));
        }
        return arrayList;
    }

    private void g(String[] strArr) {
        if (strArr != null && strArr.length != 0) {
            SQLiteDatabase G = G("Error opening database for deleteEntries.");
            if (G != null) {
                try {
                    G.delete("datalayer", String.format("%s in (%s)", new Object[]{"ID", TextUtils.join(",", Collections.nCopies(strArr.length, "?"))}), strArr);
                } catch (SQLiteException e) {
                    bh.w("Error deleting entries " + Arrays.toString(strArr));
                }
            }
        }
    }

    private List<a> iT() {
        try {
            t(this.Ty.currentTimeMillis());
            List<a> c = c(iU());
            return c;
        } finally {
            iW();
        }
    }

    private List<b> iU() {
        SQLiteDatabase G = G("Error opening database for loadSerialized.");
        List<b> arrayList = new ArrayList();
        if (G == null) {
            return arrayList;
        }
        Cursor query = G.query("datalayer", new String[]{"key", "value"}, null, null, null, null, "ID", null);
        while (query.moveToNext()) {
            try {
                arrayList.add(new b(query.getString(0), query.getBlob(1)));
            } finally {
                query.close();
            }
        }
        return arrayList;
    }

    private int iV() {
        Cursor cursor = null;
        int i = 0;
        SQLiteDatabase G = G("Error opening database for getNumStoredEntries.");
        if (G != null) {
            try {
                cursor = G.rawQuery("SELECT COUNT(*) from datalayer", null);
                if (cursor.moveToFirst()) {
                    i = (int) cursor.getLong(0);
                }
                if (cursor != null) {
                    cursor.close();
                }
            } catch (SQLiteException e) {
                bh.w("Error getting numStoredEntries");
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

    private void iW() {
        try {
            this.UF.close();
        } catch (SQLiteException e) {
        }
    }

    private Object j(byte[] bArr) {
        ObjectInputStream objectInputStream;
        Object readObject;
        Throwable th;
        ObjectInputStream objectInputStream2 = null;
        InputStream byteArrayInputStream = new ByteArrayInputStream(bArr);
        try {
            objectInputStream = new ObjectInputStream(byteArrayInputStream);
            try {
                readObject = objectInputStream.readObject();
                if (objectInputStream != null) {
                    try {
                        objectInputStream.close();
                    } catch (IOException e) {
                    }
                }
                byteArrayInputStream.close();
            } catch (IOException e2) {
                if (objectInputStream != null) {
                    try {
                        objectInputStream.close();
                    } catch (IOException e3) {
                    }
                }
                byteArrayInputStream.close();
                return readObject;
            } catch (ClassNotFoundException e4) {
                if (objectInputStream != null) {
                    try {
                        objectInputStream.close();
                    } catch (IOException e5) {
                    }
                }
                byteArrayInputStream.close();
                return readObject;
            } catch (Throwable th2) {
                th = th2;
                if (objectInputStream != null) {
                    try {
                        objectInputStream.close();
                    } catch (IOException e6) {
                        throw th;
                    }
                }
                byteArrayInputStream.close();
                throw th;
            }
        } catch (IOException e7) {
            objectInputStream = objectInputStream2;
            if (objectInputStream != null) {
                objectInputStream.close();
            }
            byteArrayInputStream.close();
            return readObject;
        } catch (ClassNotFoundException e8) {
            objectInputStream = objectInputStream2;
            if (objectInputStream != null) {
                objectInputStream.close();
            }
            byteArrayInputStream.close();
            return readObject;
        } catch (Throwable th3) {
            Throwable th4 = th3;
            objectInputStream = objectInputStream2;
            th = th4;
            if (objectInputStream != null) {
                objectInputStream.close();
            }
            byteArrayInputStream.close();
            throw th;
        }
        return readObject;
    }

    private byte[] j(Object obj) {
        ObjectOutputStream objectOutputStream;
        Throwable th;
        byte[] bArr = null;
        OutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        try {
            objectOutputStream = new ObjectOutputStream(byteArrayOutputStream);
            try {
                objectOutputStream.writeObject(obj);
                bArr = byteArrayOutputStream.toByteArray();
                if (objectOutputStream != null) {
                    try {
                        objectOutputStream.close();
                    } catch (IOException e) {
                    }
                }
                byteArrayOutputStream.close();
            } catch (IOException e2) {
                if (objectOutputStream != null) {
                    try {
                        objectOutputStream.close();
                    } catch (IOException e3) {
                    }
                }
                byteArrayOutputStream.close();
                return bArr;
            } catch (Throwable th2) {
                th = th2;
                if (objectOutputStream != null) {
                    try {
                        objectOutputStream.close();
                    } catch (IOException e4) {
                        throw th;
                    }
                }
                byteArrayOutputStream.close();
                throw th;
            }
        } catch (IOException e5) {
            objectOutputStream = bArr;
            if (objectOutputStream != null) {
                objectOutputStream.close();
            }
            byteArrayOutputStream.close();
            return bArr;
        } catch (Throwable th3) {
            Throwable th4 = th3;
            objectOutputStream = bArr;
            th = th4;
            if (objectOutputStream != null) {
                objectOutputStream.close();
            }
            byteArrayOutputStream.close();
            throw th;
        }
        return bArr;
    }

    private void t(long j) {
        SQLiteDatabase G = G("Error opening database for deleteOlderThan.");
        if (G != null) {
            try {
                bh.v("Deleted " + G.delete("datalayer", "expires <= ?", new String[]{Long.toString(j)}) + " expired items");
            } catch (SQLiteException e) {
                bh.w("Error deleting old entries.");
            }
        }
    }

    public void a(com.google.android.gms.tagmanager.DataLayer.c.a aVar) {
        this.UE.execute(new AnonymousClass2(this, aVar));
    }

    public void a(List<a> list, long j) {
        this.UE.execute(new AnonymousClass1(this, d(list), j));
    }

    public void bi(String str) {
        this.UE.execute(new AnonymousClass3(this, str));
    }
}
