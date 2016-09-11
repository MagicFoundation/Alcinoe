package com.google.android.gms.internal;

import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Looper;
import android.os.RemoteException;
import com.google.android.gms.common.GooglePlayServicesClient;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.internal.eh.e;
import com.google.android.gms.internal.eh.g;
import com.google.android.gms.panorama.Panorama.PanoramaResult;

public class ih extends eh<ig> {

    final class b extends com.google.android.gms.internal.if.a {
        final /* synthetic */ ih QG;
        private final com.google.android.gms.common.api.a.c<com.google.android.gms.panorama.Panorama.a> QH;
        private final com.google.android.gms.common.api.a.c<PanoramaResult> QI;
        private final Uri QJ;

        public b(ih ihVar, com.google.android.gms.common.api.a.c<com.google.android.gms.panorama.Panorama.a> cVar, com.google.android.gms.common.api.a.c<PanoramaResult> cVar2, Uri uri) {
            this.QG = ihVar;
            this.QH = cVar;
            this.QI = cVar2;
            this.QJ = uri;
        }

        public void a(int i, Bundle bundle, int i2, Intent intent) {
            if (this.QJ != null) {
                this.QG.getContext().revokeUriPermission(this.QJ, 1);
            }
            Status status = new Status(i, null, bundle != null ? (PendingIntent) bundle.getParcelable("pendingIntent") : null);
            if (this.QI != null) {
                this.QG.a(new c(this.QG, this.QI, status, intent));
            } else if (this.QH != null) {
                this.QG.a(new a(this.QG, this.QH, status, i2, intent));
            }
        }
    }

    final class c extends b<com.google.android.gms.common.api.a.c<PanoramaResult>> implements PanoramaResult {
        private final Status QE;
        private final Intent QF;
        final /* synthetic */ ih QG;

        public c(ih ihVar, com.google.android.gms.common.api.a.c<PanoramaResult> cVar, Status status, Intent intent) {
            this.QG = ihVar;
            super(ihVar, cVar);
            this.QE = status;
            this.QF = intent;
        }

        protected /* synthetic */ void a(Object obj) {
            c((com.google.android.gms.common.api.a.c) obj);
        }

        protected void c(com.google.android.gms.common.api.a.c<PanoramaResult> cVar) {
            cVar.b(this);
        }

        protected void cP() {
        }

        public Status getStatus() {
            return this.QE;
        }

        public Intent getViewerIntent() {
            return this.QF;
        }
    }

    final class a extends b<com.google.android.gms.common.api.a.c<com.google.android.gms.panorama.Panorama.a>> implements com.google.android.gms.panorama.Panorama.a {
        public final Status QE;
        public final Intent QF;
        final /* synthetic */ ih QG;
        public final int type;

        public a(ih ihVar, com.google.android.gms.common.api.a.c<com.google.android.gms.panorama.Panorama.a> cVar, Status status, int i, Intent intent) {
            this.QG = ihVar;
            super(ihVar, cVar);
            this.QE = status;
            this.type = i;
            this.QF = intent;
        }

        protected /* synthetic */ void a(Object obj) {
            c((com.google.android.gms.common.api.a.c) obj);
        }

        protected void c(com.google.android.gms.common.api.a.c<com.google.android.gms.panorama.Panorama.a> cVar) {
            cVar.b(this);
        }

        protected void cP() {
        }

        public Status getStatus() {
            return this.QE;
        }

        public Intent getViewerIntent() {
            return this.QF;
        }
    }

    public ih(Context context, Looper looper, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
        super(context, looper, connectionCallbacks, onConnectionFailedListener, (String[]) null);
    }

    @Deprecated
    public ih(Context context, GooglePlayServicesClient.ConnectionCallbacks connectionCallbacks, GooglePlayServicesClient.OnConnectionFailedListener onConnectionFailedListener) {
        this(context, context.getMainLooper(), new com.google.android.gms.internal.eh.c(connectionCallbacks), new g(onConnectionFailedListener));
    }

    public void a(com.google.android.gms.common.api.a.c<PanoramaResult> cVar, Uri uri, boolean z) {
        a(new b(this, null, cVar, z ? uri : null), uri, null, z);
    }

    protected void a(en enVar, e eVar) throws RemoteException {
        enVar.a(eVar, GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_VERSION_CODE, getContext().getPackageName(), new Bundle());
    }

    public void a(b bVar, Uri uri, Bundle bundle, boolean z) {
        bm();
        if (z) {
            getContext().grantUriPermission(GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_PACKAGE, uri, 1);
        }
        try {
            ((ig) eb()).a(bVar, uri, bundle, z);
        } catch (RemoteException e) {
            bVar.a(8, null, 0, null);
        }
    }

    protected String aF() {
        return "com.google.android.gms.panorama.service.START";
    }

    protected String aG() {
        return "com.google.android.gms.panorama.internal.IPanoramaService";
    }

    public ig ax(IBinder iBinder) {
        return com.google.android.gms.internal.ig.a.aw(iBinder);
    }

    public /* synthetic */ IInterface p(IBinder iBinder) {
        return ax(iBinder);
    }
}
