package android.support.v4.util;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

public class LruCache<K, V> {
    private int createCount;
    private int evictionCount;
    private int hitCount;
    private final LinkedHashMap<K, V> map;
    private int maxSize;
    private int missCount;
    private int putCount;
    private int size;

    public LruCache(int maxSize) {
        if (maxSize <= 0) {
            throw new IllegalArgumentException("maxSize <= 0");
        }
        this.maxSize = maxSize;
        this.map = new LinkedHashMap(0, 0.75f, true);
    }

    public final V get(K key) {
        if (key == null) {
            throw new NullPointerException("key == null");
        }
        synchronized (this) {
            V mapValue = this.map.get(key);
            if (mapValue != null) {
                this.hitCount++;
                return mapValue;
            }
            this.missCount++;
            V createdValue = create(key);
            if (createdValue == null) {
                return null;
            }
            synchronized (this) {
                this.createCount++;
                mapValue = this.map.put(key, createdValue);
                if (mapValue != null) {
                    this.map.put(key, mapValue);
                } else {
                    this.size += safeSizeOf(key, createdValue);
                }
            }
            if (mapValue != null) {
                entryRemoved(false, key, createdValue, mapValue);
                return mapValue;
            }
            trimToSize(this.maxSize);
            return createdValue;
        }
    }

    public final V put(K key, V value) {
        if (key == null || value == null) {
            throw new NullPointerException("key == null || value == null");
        }
        V previous;
        synchronized (this) {
            this.putCount++;
            this.size += safeSizeOf(key, value);
            previous = this.map.put(key, value);
            if (previous != null) {
                this.size -= safeSizeOf(key, previous);
            }
        }
        if (previous != null) {
            entryRemoved(false, key, previous, value);
        }
        trimToSize(this.maxSize);
        return previous;
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    public void trimToSize(int maxSize) {
        while (true) {
            synchronized (this) {
                if (this.size >= 0 && (!this.map.isEmpty() || this.size == 0)) {
                    if (this.size <= maxSize || this.map.isEmpty()) {
                    } else {
                        Entry<K, V> toEvict = (Entry) this.map.entrySet().iterator().next();
                        K key = toEvict.getKey();
                        V value = toEvict.getValue();
                        this.map.remove(key);
                        this.size -= safeSizeOf(key, value);
                        this.evictionCount++;
                        entryRemoved(true, key, value, null);
                    }
                }
            }
        }
    }

    public final V remove(K key) {
        if (key == null) {
            throw new NullPointerException("key == null");
        }
        V previous;
        synchronized (this) {
            previous = this.map.remove(key);
            if (previous != null) {
                this.size -= safeSizeOf(key, previous);
            }
        }
        if (previous != null) {
            entryRemoved(false, key, previous, null);
        }
        return previous;
    }

    protected void entryRemoved(boolean evicted, K k, V v, V v2) {
    }

    protected V create(K k) {
        return null;
    }

    private int safeSizeOf(K key, V value) {
        int result = sizeOf(key, value);
        if (result >= 0) {
            return result;
        }
        throw new IllegalStateException("Negative size: " + key + "=" + value);
    }

    protected int sizeOf(K k, V v) {
        return 1;
    }

    public final void evictAll() {
        trimToSize(-1);
    }

    public final synchronized int size() {
        return this.size;
    }

    public final synchronized int maxSize() {
        return this.maxSize;
    }

    public final synchronized int hitCount() {
        return this.hitCount;
    }

    public final synchronized int missCount() {
        return this.missCount;
    }

    public final synchronized int createCount() {
        return this.createCount;
    }

    public final synchronized int putCount() {
        return this.putCount;
    }

    public final synchronized int evictionCount() {
        return this.evictionCount;
    }

    public final synchronized Map<K, V> snapshot() {
        return new LinkedHashMap(this.map);
    }

    public final synchronized String toString() {
        String format;
        int hitPercent = 0;
        synchronized (this) {
            int accesses = this.hitCount + this.missCount;
            if (accesses != 0) {
                hitPercent = (this.hitCount * 100) / accesses;
            }
            format = String.format("LruCache[maxSize=%d,hits=%d,misses=%d,hitRate=%d%%]", new Object[]{Integer.valueOf(this.maxSize), Integer.valueOf(this.hitCount), Integer.valueOf(this.missCount), Integer.valueOf(hitPercent)});
        }
        return format;
    }
}
