package com.google.android.gms.internal;

import java.util.LinkedHashMap;
import java.util.Map.Entry;

public class ev<K, V> {
    private final LinkedHashMap<K, V> Ch;
    private int Ci;
    private int Cj;
    private int Ck;
    private int Cl;
    private int Cm;
    private int Cn;
    private int size;

    public ev(int i) {
        if (i <= 0) {
            throw new IllegalArgumentException("maxSize <= 0");
        }
        this.Ci = i;
        this.Ch = new LinkedHashMap(0, 0.75f, true);
    }

    private int c(K k, V v) {
        int sizeOf = sizeOf(k, v);
        if (sizeOf >= 0) {
            return sizeOf;
        }
        throw new IllegalStateException("Negative size: " + k + "=" + v);
    }

    protected V create(K k) {
        return null;
    }

    protected void entryRemoved(boolean evicted, K k, V v, V v2) {
    }

    public final void evictAll() {
        trimToSize(-1);
    }

    public final V get(K key) {
        if (key == null) {
            throw new NullPointerException("key == null");
        }
        synchronized (this) {
            V v = this.Ch.get(key);
            if (v != null) {
                this.Cm++;
                return v;
            }
            this.Cn++;
            V create = create(key);
            if (create == null) {
                return null;
            }
            synchronized (this) {
                this.Ck++;
                v = this.Ch.put(key, create);
                if (v != null) {
                    this.Ch.put(key, v);
                } else {
                    this.size += c(key, create);
                }
            }
            if (v != null) {
                entryRemoved(false, key, create, v);
                return v;
            }
            trimToSize(this.Ci);
            return create;
        }
    }

    public final V put(K key, V value) {
        if (key == null || value == null) {
            throw new NullPointerException("key == null || value == null");
        }
        V put;
        synchronized (this) {
            this.Cj++;
            this.size += c(key, value);
            put = this.Ch.put(key, value);
            if (put != null) {
                this.size -= c(key, put);
            }
        }
        if (put != null) {
            entryRemoved(false, key, put, value);
        }
        trimToSize(this.Ci);
        return put;
    }

    public final synchronized int size() {
        return this.size;
    }

    protected int sizeOf(K k, V v) {
        return 1;
    }

    public final synchronized String toString() {
        String format;
        int i = 0;
        synchronized (this) {
            int i2 = this.Cm + this.Cn;
            if (i2 != 0) {
                i = (this.Cm * 100) / i2;
            }
            format = String.format("LruCache[maxSize=%d,hits=%d,misses=%d,hitRate=%d%%]", new Object[]{Integer.valueOf(this.Ci), Integer.valueOf(this.Cm), Integer.valueOf(this.Cn), Integer.valueOf(i)});
        }
        return format;
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    public void trimToSize(int maxSize) {
        while (true) {
            synchronized (this) {
                if (this.size >= 0 && (!this.Ch.isEmpty() || this.size == 0)) {
                    if (this.size <= maxSize || this.Ch.isEmpty()) {
                    } else {
                        Entry entry = (Entry) this.Ch.entrySet().iterator().next();
                        Object key = entry.getKey();
                        Object value = entry.getValue();
                        this.Ch.remove(key);
                        this.size -= c(key, value);
                        this.Cl++;
                        entryRemoved(true, key, value, null);
                    }
                }
            }
        }
    }
}
