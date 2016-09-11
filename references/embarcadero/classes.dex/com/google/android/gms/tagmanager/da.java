package com.google.android.gms.tagmanager;

import com.google.android.gms.tagmanager.l.a;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

class da<K, V> implements k<K, V> {
    private final Map<K, V> Xs;
    private final int Xt;
    private final a<K, V> Xu;
    private int Xv;

    da(int i, a<K, V> aVar) {
        this.Xs = new HashMap();
        this.Xt = i;
        this.Xu = aVar;
    }

    public synchronized void e(K k, V v) {
        if (k == null || v == null) {
            throw new NullPointerException("key == null || value == null");
        }
        this.Xv += this.Xu.sizeOf(k, v);
        if (this.Xv > this.Xt) {
            Iterator it = this.Xs.entrySet().iterator();
            while (it.hasNext()) {
                Entry entry = (Entry) it.next();
                this.Xv -= this.Xu.sizeOf(entry.getKey(), entry.getValue());
                it.remove();
                if (this.Xv <= this.Xt) {
                    break;
                }
            }
        }
        this.Xs.put(k, v);
    }

    public synchronized V get(K key) {
        return this.Xs.get(key);
    }
}
