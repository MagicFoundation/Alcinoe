package com.google.android.gms.tagmanager;

import android.util.LruCache;
import com.google.android.gms.tagmanager.l.a;

class bb<K, V> implements k<K, V> {
    private LruCache<K, V> Vw;

    /* renamed from: com.google.android.gms.tagmanager.bb.1 */
    class AnonymousClass1 extends LruCache<K, V> {
        final /* synthetic */ a Vx;
        final /* synthetic */ bb Vy;

        AnonymousClass1(bb bbVar, int i, a aVar) {
            this.Vy = bbVar;
            this.Vx = aVar;
            super(i);
        }

        protected int sizeOf(K key, V value) {
            return this.Vx.sizeOf(key, value);
        }
    }

    bb(int i, a<K, V> aVar) {
        this.Vw = new AnonymousClass1(this, i, aVar);
    }

    public void e(K k, V v) {
        this.Vw.put(k, v);
    }

    public V get(K key) {
        return this.Vw.get(key);
    }
}
