package com.google.android.gms.internal;

import android.content.Context;
import android.os.Bundle;
import android.os.IBinder;
import android.os.IInterface;
import android.os.RemoteException;
import com.google.android.gms.common.GooglePlayServicesClient.ConnectionCallbacks;
import com.google.android.gms.common.GooglePlayServicesClient.OnConnectionFailedListener;
import com.google.android.gms.internal.ch.a;
import com.google.android.gms.internal.eh.e;

public class cc extends eh<ch> {
    private final int oa;

    public cc(Context context, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener, int i) {
        super(context, connectionCallbacks, onConnectionFailedListener, new String[0]);
        this.oa = i;
    }

    protected void a(en enVar, e eVar) throws RemoteException {
        enVar.g(eVar, this.oa, getContext().getPackageName(), new Bundle());
    }

    protected String aF() {
        return "com.google.android.gms.ads.service.START";
    }

    protected String aG() {
        return "com.google.android.gms.ads.internal.request.IAdRequestService";
    }

    public ch aH() {
        return (ch) super.eb();
    }

    protected ch o(IBinder iBinder) {
        return a.q(iBinder);
    }

    protected /* synthetic */ IInterface p(IBinder iBinder) {
        return o(iBinder);
    }
}
