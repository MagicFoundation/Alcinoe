package com.google.android.gms.common.api;

public interface ResultCallback<R extends Result> {
    void onResult(R r);
}
