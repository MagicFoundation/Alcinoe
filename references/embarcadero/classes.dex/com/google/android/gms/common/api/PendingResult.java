package com.google.android.gms.common.api;

import java.util.concurrent.TimeUnit;

public interface PendingResult<R extends Result> {

    public interface a {
        void k(Status status);
    }

    R await();

    R await(long j, TimeUnit timeUnit);

    void setResultCallback(ResultCallback<R> resultCallback);

    void setResultCallback(ResultCallback<R> resultCallback, long j, TimeUnit timeUnit);
}
