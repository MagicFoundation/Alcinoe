package com.google.android.gms.common.data;

import android.os.Bundle;
import java.util.Iterator;

public abstract class DataBuffer<T> implements Iterable<T> {
    protected final DataHolder zU;

    protected DataBuffer(DataHolder dataHolder) {
        this.zU = dataHolder;
        if (this.zU != null) {
            this.zU.c(this);
        }
    }

    public void close() {
        if (this.zU != null) {
            this.zU.close();
        }
    }

    public int describeContents() {
        return 0;
    }

    public abstract T get(int i);

    public int getCount() {
        return this.zU == null ? 0 : this.zU.getCount();
    }

    public Bundle getMetadata() {
        return this.zU.getMetadata();
    }

    public boolean isClosed() {
        return this.zU == null ? true : this.zU.isClosed();
    }

    public Iterator<T> iterator() {
        return new a(this);
    }
}
