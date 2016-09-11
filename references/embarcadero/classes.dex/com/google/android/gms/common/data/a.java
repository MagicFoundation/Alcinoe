package com.google.android.gms.common.data;

import com.google.android.gms.internal.er;
import java.util.Iterator;
import java.util.NoSuchElementException;

public final class a<T> implements Iterator<T> {
    private final DataBuffer<T> mDataBuffer;
    private int zV;

    public a(DataBuffer<T> dataBuffer) {
        this.mDataBuffer = (DataBuffer) er.f(dataBuffer);
        this.zV = -1;
    }

    public boolean hasNext() {
        return this.zV < this.mDataBuffer.getCount() + -1;
    }

    public T next() {
        if (hasNext()) {
            DataBuffer dataBuffer = this.mDataBuffer;
            int i = this.zV + 1;
            this.zV = i;
            return dataBuffer.get(i);
        }
        throw new NoSuchElementException("Cannot advance the iterator beyond " + this.zV);
    }

    public void remove() {
        throw new UnsupportedOperationException("Cannot remove elements from a DataBufferIterator");
    }
}
