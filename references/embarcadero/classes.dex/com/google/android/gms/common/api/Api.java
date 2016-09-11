package com.google.android.gms.common.api;

import android.content.Context;
import android.os.Looper;
import com.google.android.gms.common.api.GoogleApiClient.ApiOptions;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.internal.ee;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class Api {
    private final b<?> za;
    private final ArrayList<Scope> zb;

    public interface a {
        void connect();

        void disconnect();

        Looper getLooper();

        boolean isConnected();
    }

    public interface b<T extends a> {
        T b(Context context, Looper looper, ee eeVar, ApiOptions apiOptions, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener);

        int getPriority();
    }

    public Api(b<?> ClientBuilder, Scope... impliedScopes) {
        this.za = ClientBuilder;
        this.zb = new ArrayList(Arrays.asList(impliedScopes));
    }

    public b<?> dp() {
        return this.za;
    }

    public List<Scope> dq() {
        return this.zb;
    }
}
