package com.google.android.gms.internal;

import android.content.Context;
import android.os.Bundle;
import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.GooglePlayServicesClient.ConnectionCallbacks;
import com.google.android.gms.common.GooglePlayServicesClient.OnConnectionFailedListener;

public abstract class cb extends ct {
    private final cd mf;
    private final com.google.android.gms.internal.ca.a nY;

    public static final class a extends cb {
        private final Context mContext;

        public a(Context context, cd cdVar, com.google.android.gms.internal.ca.a aVar) {
            super(cdVar, aVar);
            this.mContext = context;
        }

        public void aD() {
        }

        public ch aE() {
            return ci.a(this.mContext, new av());
        }
    }

    public static final class b extends cb implements ConnectionCallbacks, OnConnectionFailedListener {
        private final Object mg;
        private final com.google.android.gms.internal.ca.a nY;
        private final cc nZ;

        public b(Context context, cd cdVar, com.google.android.gms.internal.ca.a aVar) {
            super(cdVar, aVar);
            this.mg = new Object();
            this.nY = aVar;
            this.nZ = new cc(context, this, this, cdVar.kN.pW);
            this.nZ.connect();
        }

        public void aD() {
            synchronized (this.mg) {
                if (this.nZ.isConnected() || this.nZ.isConnecting()) {
                    this.nZ.disconnect();
                }
            }
        }

        public ch aE() {
            ch aH;
            synchronized (this.mg) {
                try {
                    aH = this.nZ.aH();
                } catch (IllegalStateException e) {
                    aH = null;
                }
            }
            return aH;
        }

        public void onConnected(Bundle connectionHint) {
            start();
        }

        public void onConnectionFailed(ConnectionResult result) {
            this.nY.a(new cf(0));
        }

        public void onDisconnected() {
            da.s("Disconnected from remote ad request service.");
        }
    }

    public cb(cd cdVar, com.google.android.gms.internal.ca.a aVar) {
        this.mf = cdVar;
        this.nY = aVar;
    }

    private static cf a(ch chVar, cd cdVar) {
        try {
            return chVar.b(cdVar);
        } catch (Throwable e) {
            da.b("Could not fetch ad response from ad request service.", e);
            return null;
        }
    }

    public final void aB() {
        try {
            cf cfVar;
            ch aE = aE();
            if (aE == null) {
                cfVar = new cf(0);
            } else {
                cfVar = a(aE, this.mf);
                if (cfVar == null) {
                    cfVar = new cf(0);
                }
            }
            aD();
            this.nY.a(cfVar);
        } catch (Throwable th) {
            aD();
        }
    }

    public abstract void aD();

    public abstract ch aE();

    public final void onStop() {
        aD();
    }
}
