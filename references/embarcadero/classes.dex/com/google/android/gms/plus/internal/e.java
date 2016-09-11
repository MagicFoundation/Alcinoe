package com.google.android.gms.plus.internal;

import android.app.PendingIntent;
import android.content.Context;
import android.net.Uri;
import android.os.Bundle;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Looper;
import android.os.RemoteException;
import com.google.android.gms.auth.GoogleAuthUtil;
import com.google.android.gms.common.GooglePlayServicesClient;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.internal.eh;
import com.google.android.gms.internal.en;
import com.google.android.gms.internal.fh;
import com.google.android.gms.internal.io;
import com.google.android.gms.internal.ir;
import com.google.android.gms.plus.Moments.LoadMomentsResult;
import com.google.android.gms.plus.People.LoadPeopleResult;
import com.google.android.gms.plus.model.moments.Moment;
import com.google.android.gms.plus.model.moments.MomentBuffer;
import com.google.android.gms.plus.model.people.Person;
import com.google.android.gms.plus.model.people.PersonBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

public class e extends eh<d> implements GooglePlayServicesClient {
    private Person Rd;
    private final h Re;

    final class d extends b<com.google.android.gms.common.api.a.c<Status>> {
        final /* synthetic */ e Rf;
        private final Status vl;

        public d(e eVar, com.google.android.gms.common.api.a.c<Status> cVar, Status status) {
            this.Rf = eVar;
            super(eVar, cVar);
            this.vl = status;
        }

        protected /* synthetic */ void a(Object obj) {
            c((com.google.android.gms.common.api.a.c) obj);
        }

        protected void c(com.google.android.gms.common.api.a.c<Status> cVar) {
            if (cVar != null) {
                cVar.b(this.vl);
            }
        }

        protected void cP() {
        }
    }

    final class h extends b<com.google.android.gms.common.api.a.c<Status>> {
        final /* synthetic */ e Rf;
        private final Status vl;

        public h(e eVar, com.google.android.gms.common.api.a.c<Status> cVar, Status status) {
            this.Rf = eVar;
            super(eVar, cVar);
            this.vl = status;
        }

        protected /* synthetic */ void a(Object obj) {
            c((com.google.android.gms.common.api.a.c) obj);
        }

        protected void c(com.google.android.gms.common.api.a.c<Status> cVar) {
            this.Rf.disconnect();
            if (cVar != null) {
                cVar.b(this.vl);
            }
        }

        protected void cP() {
        }
    }

    final class c extends com.google.android.gms.internal.eh.d<com.google.android.gms.common.api.a.c<LoadMomentsResult>> implements LoadMomentsResult {
        private final String Dh;
        final /* synthetic */ e Rf;
        private final String Rg;
        private MomentBuffer Rh;
        private final Status vl;

        public c(e eVar, com.google.android.gms.common.api.a.c<LoadMomentsResult> cVar, Status status, DataHolder dataHolder, String str, String str2) {
            this.Rf = eVar;
            super(eVar, cVar, dataHolder);
            this.vl = status;
            this.Dh = str;
            this.Rg = str2;
        }

        protected void a(com.google.android.gms.common.api.a.c<LoadMomentsResult> cVar, DataHolder dataHolder) {
            this.Rh = dataHolder != null ? new MomentBuffer(dataHolder) : null;
            cVar.b(this);
        }

        public MomentBuffer getMomentBuffer() {
            return this.Rh;
        }

        public String getNextPageToken() {
            return this.Dh;
        }

        public Status getStatus() {
            return this.vl;
        }

        public String getUpdated() {
            return this.Rg;
        }

        public void release() {
            if (this.Rh != null) {
                this.Rh.close();
            }
        }
    }

    final class f extends com.google.android.gms.internal.eh.d<com.google.android.gms.common.api.a.c<LoadPeopleResult>> implements LoadPeopleResult {
        private final String Dh;
        final /* synthetic */ e Rf;
        private PersonBuffer Ri;
        private final Status vl;

        public f(e eVar, com.google.android.gms.common.api.a.c<LoadPeopleResult> cVar, Status status, DataHolder dataHolder, String str) {
            this.Rf = eVar;
            super(eVar, cVar, dataHolder);
            this.vl = status;
            this.Dh = str;
        }

        protected void a(com.google.android.gms.common.api.a.c<LoadPeopleResult> cVar, DataHolder dataHolder) {
            this.Ri = dataHolder != null ? new PersonBuffer(dataHolder) : null;
            cVar.b(this);
        }

        public String getNextPageToken() {
            return this.Dh;
        }

        public PersonBuffer getPersonBuffer() {
            return this.Ri;
        }

        public Status getStatus() {
            return this.vl;
        }

        public void release() {
            if (this.Ri != null) {
                this.Ri.close();
            }
        }
    }

    final class a extends a {
        private final com.google.android.gms.common.api.a.c<Status> QI;
        final /* synthetic */ e Rf;

        public a(e eVar, com.google.android.gms.common.api.a.c<Status> cVar) {
            this.Rf = eVar;
            this.QI = cVar;
        }

        public void L(Status status) {
            this.Rf.a(new d(this.Rf, this.QI, status));
        }
    }

    final class b extends a {
        private final com.google.android.gms.common.api.a.c<LoadMomentsResult> QI;
        final /* synthetic */ e Rf;

        public b(e eVar, com.google.android.gms.common.api.a.c<LoadMomentsResult> cVar) {
            this.Rf = eVar;
            this.QI = cVar;
        }

        public void a(DataHolder dataHolder, String str, String str2) {
            DataHolder dataHolder2;
            Status status = new Status(dataHolder.getStatusCode(), null, dataHolder.getMetadata() != null ? (PendingIntent) dataHolder.getMetadata().getParcelable("pendingIntent") : null);
            if (status.isSuccess() || dataHolder == null) {
                dataHolder2 = dataHolder;
            } else {
                if (!dataHolder.isClosed()) {
                    dataHolder.close();
                }
                dataHolder2 = null;
            }
            this.Rf.a(new c(this.Rf, this.QI, status, dataHolder2, str, str2));
        }
    }

    final class e extends a {
        private final com.google.android.gms.common.api.a.c<LoadPeopleResult> QI;
        final /* synthetic */ e Rf;

        public e(e eVar, com.google.android.gms.common.api.a.c<LoadPeopleResult> cVar) {
            this.Rf = eVar;
            this.QI = cVar;
        }

        public void a(DataHolder dataHolder, String str) {
            DataHolder dataHolder2;
            Status status = new Status(dataHolder.getStatusCode(), null, dataHolder.getMetadata() != null ? (PendingIntent) dataHolder.getMetadata().getParcelable("pendingIntent") : null);
            if (status.isSuccess() || dataHolder == null) {
                dataHolder2 = dataHolder;
            } else {
                if (!dataHolder.isClosed()) {
                    dataHolder.close();
                }
                dataHolder2 = null;
            }
            this.Rf.a(new f(this.Rf, this.QI, status, dataHolder2, str));
        }
    }

    final class g extends a {
        private final com.google.android.gms.common.api.a.c<Status> QI;
        final /* synthetic */ e Rf;

        public g(e eVar, com.google.android.gms.common.api.a.c<Status> cVar) {
            this.Rf = eVar;
            this.QI = cVar;
        }

        public void d(int i, Bundle bundle) {
            this.Rf.a(new h(this.Rf, this.QI, new Status(i, null, bundle != null ? (PendingIntent) bundle.getParcelable("pendingIntent") : null)));
        }
    }

    public e(Context context, Looper looper, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener, h hVar) {
        super(context, looper, connectionCallbacks, onConnectionFailedListener, hVar.hq());
        this.Re = hVar;
    }

    @Deprecated
    public e(Context context, GooglePlayServicesClient.ConnectionCallbacks connectionCallbacks, GooglePlayServicesClient.OnConnectionFailedListener onConnectionFailedListener, h hVar) {
        this(context, context.getMainLooper(), new com.google.android.gms.internal.eh.c(connectionCallbacks), new com.google.android.gms.internal.eh.g(onConnectionFailedListener), hVar);
    }

    protected void a(int i, IBinder iBinder, Bundle bundle) {
        if (i == 0 && bundle != null && bundle.containsKey("loaded_person")) {
            this.Rd = ir.i(bundle.getByteArray("loaded_person"));
        }
        super.a(i, iBinder, bundle);
    }

    public void a(com.google.android.gms.common.api.a.c<LoadPeopleResult> cVar, int i, String str) {
        bm();
        Object eVar = new e(this, cVar);
        try {
            ((d) eb()).a(eVar, 1, i, -1, str);
        } catch (RemoteException e) {
            eVar.a(DataHolder.empty(8), null);
        }
    }

    public void a(com.google.android.gms.common.api.a.c<LoadMomentsResult> cVar, int i, String str, Uri uri, String str2, String str3) {
        bm();
        Object bVar = cVar != null ? new b(this, cVar) : null;
        try {
            ((d) eb()).a(bVar, i, str, uri, str2, str3);
        } catch (RemoteException e) {
            bVar.a(DataHolder.empty(8), null, null);
        }
    }

    public void a(com.google.android.gms.common.api.a.c<Status> cVar, Moment moment) {
        bm();
        b aVar = cVar != null ? new a(this, cVar) : null;
        try {
            ((d) eb()).a(aVar, fh.a((io) moment));
        } catch (Throwable e) {
            if (aVar == null) {
                throw new IllegalStateException(e);
            }
            aVar.L(new Status(8, null, null));
        }
    }

    public void a(com.google.android.gms.common.api.a.c<LoadPeopleResult> cVar, Collection<String> collection) {
        bm();
        b eVar = new e(this, cVar);
        try {
            ((d) eb()).a(eVar, new ArrayList(collection));
        } catch (RemoteException e) {
            eVar.a(DataHolder.empty(8), null);
        }
    }

    protected void a(en enVar, com.google.android.gms.internal.eh.e eVar) throws RemoteException {
        Bundle hy = this.Re.hy();
        hy.putStringArray(GoogleAuthUtil.KEY_REQUEST_VISIBLE_ACTIVITIES, this.Re.hr());
        enVar.a(eVar, GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_VERSION_CODE, this.Re.hu(), this.Re.ht(), ea(), this.Re.getAccountName(), hy);
    }

    protected d aB(IBinder iBinder) {
        return com.google.android.gms.plus.internal.d.a.aA(iBinder);
    }

    protected String aF() {
        return "com.google.android.gms.plus.service.START";
    }

    protected String aG() {
        return "com.google.android.gms.plus.internal.IPlusService";
    }

    public boolean aR(String str) {
        return Arrays.asList(ea()).contains(str);
    }

    public void c(com.google.android.gms.common.api.a.c<LoadPeopleResult> cVar, String[] strArr) {
        a((com.google.android.gms.common.api.a.c) cVar, Arrays.asList(strArr));
    }

    public void clearDefaultAccount() {
        bm();
        try {
            this.Rd = null;
            ((d) eb()).clearDefaultAccount();
        } catch (Throwable e) {
            throw new IllegalStateException(e);
        }
    }

    public String getAccountName() {
        bm();
        try {
            return ((d) eb()).getAccountName();
        } catch (Throwable e) {
            throw new IllegalStateException(e);
        }
    }

    public Person getCurrentPerson() {
        bm();
        return this.Rd;
    }

    public void i(com.google.android.gms.common.api.a.c<LoadMomentsResult> cVar) {
        a(cVar, 20, null, null, null, "me");
    }

    public void i(com.google.android.gms.common.api.a.c<LoadPeopleResult> cVar, String str) {
        a((com.google.android.gms.common.api.a.c) cVar, 0, str);
    }

    public void j(com.google.android.gms.common.api.a.c<LoadPeopleResult> cVar) {
        bm();
        Object eVar = new e(this, cVar);
        try {
            ((d) eb()).a(eVar, 2, 1, -1, null);
        } catch (RemoteException e) {
            eVar.a(DataHolder.empty(8), null);
        }
    }

    public void k(com.google.android.gms.common.api.a.c<Status> cVar) {
        bm();
        clearDefaultAccount();
        Object gVar = new g(this, cVar);
        try {
            ((d) eb()).b(gVar);
        } catch (RemoteException e) {
            gVar.d(8, null);
        }
    }

    protected /* synthetic */ IInterface p(IBinder iBinder) {
        return aB(iBinder);
    }

    public void removeMoment(String momentId) {
        bm();
        try {
            ((d) eb()).removeMoment(momentId);
        } catch (Throwable e) {
            throw new IllegalStateException(e);
        }
    }
}
