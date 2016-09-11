package com.google.android.gms.drive.internal;

import android.content.Context;
import android.os.Bundle;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Looper;
import android.os.RemoteException;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.drive.DriveId;
import com.google.android.gms.drive.events.DriveEvent;
import com.google.android.gms.drive.events.DriveEvent.Listener;
import com.google.android.gms.drive.events.c;
import com.google.android.gms.drive.internal.u.a;
import com.google.android.gms.internal.ee;
import com.google.android.gms.internal.eh;
import com.google.android.gms.internal.eh.e;
import com.google.android.gms.internal.en;
import com.google.android.gms.internal.er;
import java.util.HashMap;
import java.util.Map;

public class n extends eh<u> {
    private DriveId DA;
    final ConnectionCallbacks DB;
    Map<DriveId, Map<Listener<?>, s<?>>> DC;
    private DriveId Dz;
    private final String vi;

    /* renamed from: com.google.android.gms.drive.internal.n.1 */
    class AnonymousClass1 extends j {
        final /* synthetic */ DriveId DD;
        final /* synthetic */ int DE;
        final /* synthetic */ s DF;
        final /* synthetic */ n DG;

        AnonymousClass1(n nVar, DriveId driveId, int i, s sVar) {
            this.DG = nVar;
            this.DD = driveId;
            this.DE = i;
            this.DF = sVar;
        }

        protected void a(n nVar) throws RemoteException {
            nVar.eT().a(new AddEventListenerRequest(this.DD, this.DE), this.DF, null, new ak(this));
        }
    }

    /* renamed from: com.google.android.gms.drive.internal.n.2 */
    class AnonymousClass2 extends j {
        final /* synthetic */ DriveId DD;
        final /* synthetic */ int DE;
        final /* synthetic */ s DF;
        final /* synthetic */ n DG;

        AnonymousClass2(n nVar, DriveId driveId, int i, s sVar) {
            this.DG = nVar;
            this.DD = driveId;
            this.DE = i;
            this.DF = sVar;
        }

        protected void a(n nVar) throws RemoteException {
            nVar.eT().a(new RemoveEventListenerRequest(this.DD, this.DE), this.DF, null, new ak(this));
        }
    }

    public n(Context context, Looper looper, ee eeVar, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener, String[] strArr) {
        super(context, looper, connectionCallbacks, onConnectionFailedListener, strArr);
        this.DC = new HashMap();
        this.vi = (String) er.b(eeVar.dR(), (Object) "Must call Api.ClientBuilder.setAccountName()");
        this.DB = connectionCallbacks;
    }

    protected u C(IBinder iBinder) {
        return a.D(iBinder);
    }

    <C extends DriveEvent> PendingResult<Status> a(GoogleApiClient googleApiClient, DriveId driveId, int i, Listener<C> listener) {
        PendingResult<Status> kVar;
        er.b(c.a(i, driveId), (Object) "id");
        er.b((Object) listener, (Object) "listener");
        er.a(isConnected(), "Client must be connected");
        synchronized (this.DC) {
            Map map = (Map) this.DC.get(driveId);
            if (map == null) {
                map = new HashMap();
                this.DC.put(driveId, map);
            }
            if (map.containsKey(listener)) {
                kVar = new k(Status.zQ);
            } else {
                s sVar = new s(getLooper(), i, listener);
                map.put(listener, sVar);
                kVar = googleApiClient.b(new AnonymousClass1(this, driveId, i, sVar));
            }
        }
        return kVar;
    }

    protected void a(int i, IBinder iBinder, Bundle bundle) {
        if (bundle != null) {
            bundle.setClassLoader(getClass().getClassLoader());
            this.Dz = (DriveId) bundle.getParcelable("com.google.android.gms.drive.root_id");
            this.DA = (DriveId) bundle.getParcelable("com.google.android.gms.drive.appdata_id");
        }
        super.a(i, iBinder, bundle);
    }

    protected void a(en enVar, e eVar) throws RemoteException {
        String packageName = getContext().getPackageName();
        er.f(eVar);
        er.f(packageName);
        er.f(ea());
        enVar.a(eVar, GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_VERSION_CODE, packageName, ea(), this.vi, new Bundle());
    }

    protected String aF() {
        return "com.google.android.gms.drive.ApiService.START";
    }

    protected String aG() {
        return "com.google.android.gms.drive.internal.IDriveService";
    }

    PendingResult<Status> b(GoogleApiClient googleApiClient, DriveId driveId, int i, Listener<?> listener) {
        PendingResult<Status> kVar;
        er.b(c.a(i, driveId), (Object) "id");
        er.b((Object) listener, (Object) "listener");
        er.a(isConnected(), "Client must be connected");
        synchronized (this.DC) {
            Map map = (Map) this.DC.get(driveId);
            if (map == null) {
                kVar = new k(Status.zQ);
            } else {
                s sVar = (s) map.remove(listener);
                if (sVar == null) {
                    kVar = new k(Status.zQ);
                } else {
                    if (map.isEmpty()) {
                        this.DC.remove(driveId);
                    }
                    kVar = googleApiClient.b(new AnonymousClass2(this, driveId, i, sVar));
                }
            }
        }
        return kVar;
    }

    public void disconnect() {
        u uVar = (u) eb();
        if (uVar != null) {
            try {
                uVar.a(new DisconnectRequest());
            } catch (RemoteException e) {
            }
        }
        super.disconnect();
        this.DC.clear();
    }

    public u eT() {
        return (u) eb();
    }

    public DriveId eU() {
        return this.Dz;
    }

    public DriveId eV() {
        return this.DA;
    }

    protected /* synthetic */ IInterface p(IBinder iBinder) {
        return C(iBinder);
    }
}
