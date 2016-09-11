package com.google.android.gms.tagmanager;

import android.os.Build.VERSION;

class l<K, V> {
    final a<K, V> TK;

    public interface a<K, V> {
        int sizeOf(K k, V v);
    }

    public l() {
        this.TK = new a<K, V>() {
            final /* synthetic */ l TL;

            {
                this.TL = r1;
            }

            public int sizeOf(K k, V v) {
                return 1;
            }
        };
    }

    public k<K, V> a(int i, a<K, V> aVar) {
        if (i > 0) {
            return iA() < 12 ? new da(i, aVar) : new bb(i, aVar);
        } else {
            throw new IllegalArgumentException("maxSize <= 0");
        }
    }

    int iA() {
        return VERSION.SDK_INT;
    }
}
