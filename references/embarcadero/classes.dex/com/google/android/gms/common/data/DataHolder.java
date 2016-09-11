package com.google.android.gms.common.data;

import android.content.ContentValues;
import android.database.AbstractWindowedCursor;
import android.database.CharArrayBuffer;
import android.database.CursorIndexOutOfBoundsException;
import android.database.CursorWindow;
import android.net.Uri;
import android.os.Bundle;
import android.os.Parcel;
import android.util.Log;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.internal.ed;
import com.google.android.gms.internal.ep;
import com.google.android.gms.internal.er;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

public final class DataHolder implements SafeParcelable {
    private static final Builder Ai;
    public static final DataHolderCreator CREATOR;
    private final String[] Aa;
    Bundle Ab;
    private final CursorWindow[] Ac;
    private final Bundle Ad;
    int[] Ae;
    int Af;
    private Object Ag;
    private boolean Ah;
    boolean mClosed;
    private final int wj;
    private final int yJ;

    public static class Builder {
        private final String[] Aa;
        private final ArrayList<HashMap<String, Object>> Aj;
        private final String Ak;
        private final HashMap<Object, Integer> Al;
        private boolean Am;
        private String An;

        private Builder(String[] columns, String uniqueColumn) {
            this.Aa = (String[]) er.f(columns);
            this.Aj = new ArrayList();
            this.Ak = uniqueColumn;
            this.Al = new HashMap();
            this.Am = false;
            this.An = null;
        }

        private void a(HashMap<String, Object> hashMap) {
            Object obj = hashMap.get(this.Ak);
            if (obj != null) {
                Integer num = (Integer) this.Al.remove(obj);
                if (num != null) {
                    this.Aj.remove(num.intValue());
                }
                this.Al.put(obj, Integer.valueOf(this.Aj.size()));
            }
        }

        private void dJ() {
            if (this.Ak != null) {
                this.Al.clear();
                int size = this.Aj.size();
                for (int i = 0; i < size; i++) {
                    Object obj = ((HashMap) this.Aj.get(i)).get(this.Ak);
                    if (obj != null) {
                        this.Al.put(obj, Integer.valueOf(i));
                    }
                }
            }
        }

        public DataHolder build(int statusCode) {
            return new DataHolder(statusCode, null, null);
        }

        public DataHolder build(int statusCode, Bundle metadata) {
            return new DataHolder(statusCode, metadata, -1, null);
        }

        public DataHolder build(int statusCode, Bundle metadata, int maxResults) {
            return new DataHolder(statusCode, metadata, maxResults, null);
        }

        public int getCount() {
            return this.Aj.size();
        }

        public Builder removeRowsWithValue(String column, Object value) {
            for (int size = this.Aj.size() - 1; size >= 0; size--) {
                if (ep.equal(((HashMap) this.Aj.get(size)).get(column), value)) {
                    this.Aj.remove(size);
                }
            }
            return this;
        }

        public Builder sort(String sortColumn) {
            ed.d(sortColumn);
            if (!(this.Am && sortColumn.equals(this.An))) {
                Collections.sort(this.Aj, new a(sortColumn));
                dJ();
                this.Am = true;
                this.An = sortColumn;
            }
            return this;
        }

        public Builder withRow(ContentValues values) {
            ed.d(values);
            HashMap hashMap = new HashMap(values.size());
            for (Entry entry : values.valueSet()) {
                hashMap.put(entry.getKey(), entry.getValue());
            }
            return withRow(hashMap);
        }

        public Builder withRow(HashMap<String, Object> row) {
            ed.d(row);
            if (this.Ak != null) {
                a((HashMap) row);
            }
            this.Aj.add(row);
            this.Am = false;
            return this;
        }
    }

    private static final class a implements Comparator<HashMap<String, Object>> {
        private final String Ao;

        a(String str) {
            this.Ao = (String) er.f(str);
        }

        public int a(HashMap<String, Object> hashMap, HashMap<String, Object> hashMap2) {
            Object f = er.f(hashMap.get(this.Ao));
            Object f2 = er.f(hashMap2.get(this.Ao));
            if (f.equals(f2)) {
                return 0;
            }
            if (f instanceof Boolean) {
                return ((Boolean) f).compareTo((Boolean) f2);
            }
            if (f instanceof Long) {
                return ((Long) f).compareTo((Long) f2);
            }
            if (f instanceof Integer) {
                return ((Integer) f).compareTo((Integer) f2);
            }
            if (f instanceof String) {
                return ((String) f).compareTo((String) f2);
            }
            throw new IllegalArgumentException("Unknown type for lValue " + f);
        }

        public /* synthetic */ int compare(Object x0, Object x1) {
            return a((HashMap) x0, (HashMap) x1);
        }
    }

    /* renamed from: com.google.android.gms.common.data.DataHolder.1 */
    static class AnonymousClass1 extends Builder {
        AnonymousClass1(String[] strArr, String str) {
            super(str, null);
        }

        public Builder withRow(ContentValues values) {
            throw new UnsupportedOperationException("Cannot add data to empty builder");
        }

        public Builder withRow(HashMap<String, Object> hashMap) {
            throw new UnsupportedOperationException("Cannot add data to empty builder");
        }
    }

    static {
        CREATOR = new DataHolderCreator();
        Ai = new AnonymousClass1(new String[0], null);
    }

    DataHolder(int versionCode, String[] columns, CursorWindow[] windows, int statusCode, Bundle metadata) {
        this.mClosed = false;
        this.Ah = true;
        this.wj = versionCode;
        this.Aa = columns;
        this.Ac = windows;
        this.yJ = statusCode;
        this.Ad = metadata;
    }

    public DataHolder(AbstractWindowedCursor cursor, int statusCode, Bundle metadata) {
        this(cursor.getColumnNames(), a(cursor), statusCode, metadata);
    }

    private DataHolder(Builder builder, int statusCode, Bundle metadata) {
        this(builder.Aa, a(builder, -1), statusCode, metadata);
    }

    private DataHolder(Builder builder, int statusCode, Bundle metadata, int maxResults) {
        this(builder.Aa, a(builder, maxResults), statusCode, metadata);
    }

    public DataHolder(String[] columns, CursorWindow[] windows, int statusCode, Bundle metadata) {
        this.mClosed = false;
        this.Ah = true;
        this.wj = 1;
        this.Aa = (String[]) er.f(columns);
        this.Ac = (CursorWindow[]) er.f(windows);
        this.yJ = statusCode;
        this.Ad = metadata;
        validateContents();
    }

    private static CursorWindow[] a(AbstractWindowedCursor abstractWindowedCursor) {
        int i;
        ArrayList arrayList = new ArrayList();
        int count = abstractWindowedCursor.getCount();
        CursorWindow window = abstractWindowedCursor.getWindow();
        if (window == null || window.getStartPosition() != 0) {
            i = 0;
        } else {
            window.acquireReference();
            abstractWindowedCursor.setWindow(null);
            arrayList.add(window);
            i = window.getNumRows();
        }
        while (i < count && abstractWindowedCursor.moveToPosition(i)) {
            CursorWindow window2 = abstractWindowedCursor.getWindow();
            if (window2 != null) {
                window2.acquireReference();
                abstractWindowedCursor.setWindow(null);
            } else {
                try {
                    window2 = new CursorWindow(false);
                    window2.setStartPosition(i);
                    abstractWindowedCursor.fillWindow(i, window2);
                } catch (Throwable th) {
                    abstractWindowedCursor.close();
                }
            }
            if (window2.getNumRows() == 0) {
                break;
            }
            arrayList.add(window2);
            i = window2.getNumRows() + window2.getStartPosition();
        }
        abstractWindowedCursor.close();
        return (CursorWindow[]) arrayList.toArray(new CursorWindow[arrayList.size()]);
    }

    private static CursorWindow[] a(Builder builder, int i) {
        int size;
        int i2 = 0;
        if (builder.Aa.length == 0) {
            return new CursorWindow[0];
        }
        List b = (i < 0 || i >= builder.Aj.size()) ? builder.Aj : builder.Aj.subList(0, i);
        int size2 = b.size();
        CursorWindow cursorWindow = new CursorWindow(false);
        ArrayList arrayList = new ArrayList();
        arrayList.add(cursorWindow);
        cursorWindow.setNumColumns(builder.Aa.length);
        int i3 = 0;
        int i4 = 0;
        while (i3 < size2) {
            int i5;
            int i6;
            CursorWindow cursorWindow2;
            if (cursorWindow.allocRow()) {
                i5 = i4;
            } else {
                Log.d("DataHolder", "Allocating additional cursor window for large data set (row " + i3 + ")");
                cursorWindow = new CursorWindow(false);
                cursorWindow.setStartPosition(i3);
                cursorWindow.setNumColumns(builder.Aa.length);
                arrayList.add(cursorWindow);
                if (cursorWindow.allocRow()) {
                    i5 = 0;
                } else {
                    Log.e("DataHolder", "Unable to allocate row to hold data.");
                    arrayList.remove(cursorWindow);
                    return (CursorWindow[]) arrayList.toArray(new CursorWindow[arrayList.size()]);
                }
            }
            Map map = (Map) b.get(i3);
            boolean z = true;
            for (int i7 = 0; i7 < builder.Aa.length && z; i7++) {
                String str = builder.Aa[i7];
                Object obj = map.get(str);
                if (obj == null) {
                    z = cursorWindow.putNull(i5, i7);
                } else if (obj instanceof String) {
                    z = cursorWindow.putString((String) obj, i5, i7);
                } else if (obj instanceof Long) {
                    z = cursorWindow.putLong(((Long) obj).longValue(), i5, i7);
                } else if (obj instanceof Integer) {
                    z = cursorWindow.putLong((long) ((Integer) obj).intValue(), i5, i7);
                } else if (obj instanceof Boolean) {
                    z = cursorWindow.putLong(((Boolean) obj).booleanValue() ? 1 : 0, i5, i7);
                } else if (obj instanceof byte[]) {
                    z = cursorWindow.putBlob((byte[]) obj, i5, i7);
                } else {
                    throw new IllegalArgumentException("Unsupported object for column " + str + ": " + obj);
                }
            }
            if (z) {
                i6 = i5 + 1;
                i4 = i3;
                cursorWindow2 = cursorWindow;
            } else {
                try {
                    Log.d("DataHolder", "Couldn't populate window data for row " + i3 + " - allocating new window.");
                    cursorWindow.freeLastRow();
                    CursorWindow cursorWindow3 = new CursorWindow(false);
                    cursorWindow3.setNumColumns(builder.Aa.length);
                    arrayList.add(cursorWindow3);
                    i4 = i3 - 1;
                    cursorWindow2 = cursorWindow3;
                    i6 = 0;
                } catch (RuntimeException e) {
                    RuntimeException runtimeException = e;
                    size = arrayList.size();
                    while (i2 < size) {
                        ((CursorWindow) arrayList.get(i2)).close();
                        i2++;
                    }
                    throw runtimeException;
                }
            }
            cursorWindow = cursorWindow2;
            i3 = i4 + 1;
            i4 = i6;
        }
        return (CursorWindow[]) arrayList.toArray(new CursorWindow[arrayList.size()]);
    }

    public static Builder builder(String[] columns) {
        return new Builder(null, null);
    }

    public static Builder builder(String[] columns, String uniqueColumn) {
        er.f(uniqueColumn);
        return new Builder(uniqueColumn, null);
    }

    private void e(String str, int i) {
        if (this.Ab == null || !this.Ab.containsKey(str)) {
            throw new IllegalArgumentException("No such column: " + str);
        } else if (isClosed()) {
            throw new IllegalArgumentException("Buffer is closed.");
        } else if (i < 0 || i >= this.Af) {
            throw new CursorIndexOutOfBoundsException(i, this.Af);
        }
    }

    public static DataHolder empty(int statusCode) {
        return empty(statusCode, null);
    }

    public static DataHolder empty(int statusCode, Bundle metadata) {
        return new DataHolder(Ai, statusCode, metadata);
    }

    public int I(int i) {
        int i2 = 0;
        boolean z = i >= 0 && i < this.Af;
        er.v(z);
        while (i2 < this.Ae.length) {
            if (i < this.Ae[i2]) {
                i2--;
                break;
            }
            i2++;
        }
        return i2 == this.Ae.length ? i2 - 1 : i2;
    }

    public void c(Object obj) {
        this.Ag = obj;
    }

    public void close() {
        synchronized (this) {
            if (!this.mClosed) {
                this.mClosed = true;
                for (CursorWindow close : this.Ac) {
                    close.close();
                }
            }
        }
    }

    public void copyToBuffer(String column, int row, int windowIndex, CharArrayBuffer dataOut) {
        e(column, row);
        this.Ac[windowIndex].copyStringToBuffer(row, this.Ab.getInt(column), dataOut);
    }

    String[] dH() {
        return this.Aa;
    }

    CursorWindow[] dI() {
        return this.Ac;
    }

    public int describeContents() {
        return 0;
    }

    protected void finalize() throws Throwable {
        try {
            if (this.Ah && this.Ac.length > 0 && !isClosed()) {
                Log.e("DataBuffer", "Internal data leak within a DataBuffer object detected!  Be sure to explicitly call close() on all DataBuffer extending objects when you are done with them. (" + (this.Ag == null ? "internal object: " + toString() : this.Ag.toString()) + ")");
                close();
            }
            super.finalize();
        } catch (Throwable th) {
            super.finalize();
        }
    }

    public boolean getBoolean(String column, int row, int windowIndex) {
        e(column, row);
        return Long.valueOf(this.Ac[windowIndex].getLong(row, this.Ab.getInt(column))).longValue() == 1;
    }

    public byte[] getByteArray(String column, int row, int windowIndex) {
        e(column, row);
        return this.Ac[windowIndex].getBlob(row, this.Ab.getInt(column));
    }

    public int getCount() {
        return this.Af;
    }

    public int getInteger(String column, int row, int windowIndex) {
        e(column, row);
        return this.Ac[windowIndex].getInt(row, this.Ab.getInt(column));
    }

    public long getLong(String column, int row, int windowIndex) {
        e(column, row);
        return this.Ac[windowIndex].getLong(row, this.Ab.getInt(column));
    }

    public Bundle getMetadata() {
        return this.Ad;
    }

    public int getStatusCode() {
        return this.yJ;
    }

    public String getString(String column, int row, int windowIndex) {
        e(column, row);
        return this.Ac[windowIndex].getString(row, this.Ab.getInt(column));
    }

    int getVersionCode() {
        return this.wj;
    }

    public boolean hasColumn(String column) {
        return this.Ab.containsKey(column);
    }

    public boolean hasNull(String column, int row, int windowIndex) {
        e(column, row);
        return this.Ac[windowIndex].isNull(row, this.Ab.getInt(column));
    }

    public boolean isClosed() {
        boolean z;
        synchronized (this) {
            z = this.mClosed;
        }
        return z;
    }

    public Uri parseUri(String column, int row, int windowIndex) {
        String string = getString(column, row, windowIndex);
        return string == null ? null : Uri.parse(string);
    }

    public void validateContents() {
        int i;
        int i2 = 0;
        this.Ab = new Bundle();
        for (i = 0; i < this.Aa.length; i++) {
            this.Ab.putInt(this.Aa[i], i);
        }
        this.Ae = new int[this.Ac.length];
        i = 0;
        while (i2 < this.Ac.length) {
            this.Ae[i2] = i;
            i += this.Ac[i2].getNumRows() - (i - this.Ac[i2].getStartPosition());
            i2++;
        }
        this.Af = i;
    }

    public void writeToParcel(Parcel dest, int flags) {
        DataHolderCreator.a(this, dest, flags);
    }
}
