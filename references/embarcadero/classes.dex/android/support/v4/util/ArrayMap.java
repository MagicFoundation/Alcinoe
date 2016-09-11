package android.support.v4.util;

import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

public class ArrayMap<K, V> extends SimpleArrayMap<K, V> implements Map<K, V> {
    MapCollections<K, V> mCollections;

    public ArrayMap(int capacity) {
        super(capacity);
    }

    public ArrayMap(SimpleArrayMap map) {
        super(map);
    }

    private MapCollections<K, V> getCollection() {
        if (this.mCollections == null) {
            this.mCollections = new MapCollections<K, V>() {
                protected int colGetSize() {
                    return ArrayMap.this.mSize;
                }

                protected Object colGetEntry(int index, int offset) {
                    return ArrayMap.this.mArray[(index << 1) + offset];
                }

                protected int colIndexOfKey(Object key) {
                    return key == null ? ArrayMap.this.indexOfNull() : ArrayMap.this.indexOf(key, key.hashCode());
                }

                protected int colIndexOfValue(Object value) {
                    return ArrayMap.this.indexOfValue(value);
                }

                protected Map<K, V> colGetMap() {
                    return ArrayMap.this;
                }

                protected void colPut(K key, V value) {
                    ArrayMap.this.put(key, value);
                }

                protected V colSetValue(int index, V value) {
                    return ArrayMap.this.setValueAt(index, value);
                }

                protected void colRemoveAt(int index) {
                    ArrayMap.this.removeAt(index);
                }

                protected void colClear() {
                    ArrayMap.this.clear();
                }
            };
        }
        return this.mCollections;
    }

    public boolean containsAll(Collection<?> collection) {
        return MapCollections.containsAllHelper(this, collection);
    }

    public void putAll(Map<? extends K, ? extends V> map) {
        ensureCapacity(this.mSize + map.size());
        for (Entry<? extends K, ? extends V> entry : map.entrySet()) {
            put(entry.getKey(), entry.getValue());
        }
    }

    public boolean removeAll(Collection<?> collection) {
        return MapCollections.removeAllHelper(this, collection);
    }

    public boolean retainAll(Collection<?> collection) {
        return MapCollections.retainAllHelper(this, collection);
    }

    public Set<Entry<K, V>> entrySet() {
        return getCollection().getEntrySet();
    }

    public Set<K> keySet() {
        return getCollection().getKeySet();
    }

    public Collection<V> values() {
        return getCollection().getValues();
    }
}
