package com.google.android.gms.common.data;

public interface Freezable<T> {
    T freeze();

    boolean isDataValid();
}
