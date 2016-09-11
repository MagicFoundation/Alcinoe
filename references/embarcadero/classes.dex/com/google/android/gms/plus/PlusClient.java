package com.google.android.gms.plus;

import android.content.Context;
import android.net.Uri;
import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.GooglePlayServicesClient;
import com.google.android.gms.common.GooglePlayServicesClient.ConnectionCallbacks;
import com.google.android.gms.common.GooglePlayServicesClient.OnConnectionFailedListener;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.common.api.a.c;
import com.google.android.gms.plus.Moments.LoadMomentsResult;
import com.google.android.gms.plus.People.LoadPeopleResult;
import com.google.android.gms.plus.internal.e;
import com.google.android.gms.plus.internal.i;
import com.google.android.gms.plus.model.moments.Moment;
import com.google.android.gms.plus.model.moments.MomentBuffer;
import com.google.android.gms.plus.model.people.Person;
import com.google.android.gms.plus.model.people.PersonBuffer;
import java.util.Collection;

@Deprecated
public class PlusClient implements GooglePlayServicesClient {
    final e QN;

    @Deprecated
    public static class Builder {
        private final ConnectionCallbacks QS;
        private final OnConnectionFailedListener QT;
        private final i QU;
        private final Context mContext;

        public Builder(Context context, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener connectionFailedListener) {
            this.mContext = context;
            this.QS = connectionCallbacks;
            this.QT = connectionFailedListener;
            this.QU = new i(this.mContext);
        }

        public PlusClient build() {
            return new PlusClient(new e(this.mContext, this.QS, this.QT, this.QU.hA()));
        }

        public Builder clearScopes() {
            this.QU.hz();
            return this;
        }

        public Builder setAccountName(String accountName) {
            this.QU.aS(accountName);
            return this;
        }

        public Builder setActions(String... actions) {
            this.QU.f(actions);
            return this;
        }

        public Builder setScopes(String... scopes) {
            this.QU.e(scopes);
            return this;
        }
    }

    @Deprecated
    public interface OnAccessRevokedListener {
        void onAccessRevoked(ConnectionResult connectionResult);
    }

    @Deprecated
    public interface OnMomentsLoadedListener {
        @Deprecated
        void onMomentsLoaded(ConnectionResult connectionResult, MomentBuffer momentBuffer, String str, String str2);
    }

    @Deprecated
    public interface OnPeopleLoadedListener {
        void onPeopleLoaded(ConnectionResult connectionResult, PersonBuffer personBuffer, String str);
    }

    @Deprecated
    public interface OrderBy {
        @Deprecated
        public static final int ALPHABETICAL = 0;
        @Deprecated
        public static final int BEST = 1;
    }

    /* renamed from: com.google.android.gms.plus.PlusClient.1 */
    class AnonymousClass1 implements c<LoadMomentsResult> {
        final /* synthetic */ OnMomentsLoadedListener QO;
        final /* synthetic */ PlusClient QP;

        AnonymousClass1(PlusClient plusClient, OnMomentsLoadedListener onMomentsLoadedListener) {
            this.QP = plusClient;
            this.QO = onMomentsLoadedListener;
        }

        public void a(LoadMomentsResult loadMomentsResult) {
            this.QO.onMomentsLoaded(loadMomentsResult.getStatus().dG(), loadMomentsResult.getMomentBuffer(), loadMomentsResult.getNextPageToken(), loadMomentsResult.getUpdated());
        }

        public /* synthetic */ void b(Object obj) {
            a((LoadMomentsResult) obj);
        }
    }

    /* renamed from: com.google.android.gms.plus.PlusClient.2 */
    class AnonymousClass2 implements c<LoadMomentsResult> {
        final /* synthetic */ OnMomentsLoadedListener QO;
        final /* synthetic */ PlusClient QP;

        AnonymousClass2(PlusClient plusClient, OnMomentsLoadedListener onMomentsLoadedListener) {
            this.QP = plusClient;
            this.QO = onMomentsLoadedListener;
        }

        public void a(LoadMomentsResult loadMomentsResult) {
            this.QO.onMomentsLoaded(loadMomentsResult.getStatus().dG(), loadMomentsResult.getMomentBuffer(), loadMomentsResult.getNextPageToken(), loadMomentsResult.getUpdated());
        }

        public /* synthetic */ void b(Object obj) {
            a((LoadMomentsResult) obj);
        }
    }

    /* renamed from: com.google.android.gms.plus.PlusClient.3 */
    class AnonymousClass3 implements c<LoadPeopleResult> {
        final /* synthetic */ PlusClient QP;
        final /* synthetic */ OnPeopleLoadedListener QQ;

        AnonymousClass3(PlusClient plusClient, OnPeopleLoadedListener onPeopleLoadedListener) {
            this.QP = plusClient;
            this.QQ = onPeopleLoadedListener;
        }

        public void a(LoadPeopleResult loadPeopleResult) {
            this.QQ.onPeopleLoaded(loadPeopleResult.getStatus().dG(), loadPeopleResult.getPersonBuffer(), loadPeopleResult.getNextPageToken());
        }

        public /* synthetic */ void b(Object obj) {
            a((LoadPeopleResult) obj);
        }
    }

    /* renamed from: com.google.android.gms.plus.PlusClient.4 */
    class AnonymousClass4 implements c<LoadPeopleResult> {
        final /* synthetic */ PlusClient QP;
        final /* synthetic */ OnPeopleLoadedListener QQ;

        AnonymousClass4(PlusClient plusClient, OnPeopleLoadedListener onPeopleLoadedListener) {
            this.QP = plusClient;
            this.QQ = onPeopleLoadedListener;
        }

        public void a(LoadPeopleResult loadPeopleResult) {
            this.QQ.onPeopleLoaded(loadPeopleResult.getStatus().dG(), loadPeopleResult.getPersonBuffer(), loadPeopleResult.getNextPageToken());
        }

        public /* synthetic */ void b(Object obj) {
            a((LoadPeopleResult) obj);
        }
    }

    /* renamed from: com.google.android.gms.plus.PlusClient.5 */
    class AnonymousClass5 implements c<LoadPeopleResult> {
        final /* synthetic */ PlusClient QP;
        final /* synthetic */ OnPeopleLoadedListener QQ;

        AnonymousClass5(PlusClient plusClient, OnPeopleLoadedListener onPeopleLoadedListener) {
            this.QP = plusClient;
            this.QQ = onPeopleLoadedListener;
        }

        public void a(LoadPeopleResult loadPeopleResult) {
            this.QQ.onPeopleLoaded(loadPeopleResult.getStatus().dG(), loadPeopleResult.getPersonBuffer(), loadPeopleResult.getNextPageToken());
        }

        public /* synthetic */ void b(Object obj) {
            a((LoadPeopleResult) obj);
        }
    }

    /* renamed from: com.google.android.gms.plus.PlusClient.6 */
    class AnonymousClass6 implements c<LoadPeopleResult> {
        final /* synthetic */ PlusClient QP;
        final /* synthetic */ OnPeopleLoadedListener QQ;

        AnonymousClass6(PlusClient plusClient, OnPeopleLoadedListener onPeopleLoadedListener) {
            this.QP = plusClient;
            this.QQ = onPeopleLoadedListener;
        }

        public void a(LoadPeopleResult loadPeopleResult) {
            this.QQ.onPeopleLoaded(loadPeopleResult.getStatus().dG(), loadPeopleResult.getPersonBuffer(), loadPeopleResult.getNextPageToken());
        }

        public /* synthetic */ void b(Object obj) {
            a((LoadPeopleResult) obj);
        }
    }

    /* renamed from: com.google.android.gms.plus.PlusClient.7 */
    class AnonymousClass7 implements c<Status> {
        final /* synthetic */ PlusClient QP;
        final /* synthetic */ OnAccessRevokedListener QR;

        AnonymousClass7(PlusClient plusClient, OnAccessRevokedListener onAccessRevokedListener) {
            this.QP = plusClient;
            this.QR = onAccessRevokedListener;
        }

        public void K(Status status) {
            this.QR.onAccessRevoked(status.getStatus().dG());
        }

        public /* synthetic */ void b(Object obj) {
            K((Status) obj);
        }
    }

    PlusClient(e plusClientImpl) {
        this.QN = plusClientImpl;
    }

    @Deprecated
    public void clearDefaultAccount() {
        this.QN.clearDefaultAccount();
    }

    @Deprecated
    public void connect() {
        this.QN.connect();
    }

    @Deprecated
    public void disconnect() {
        this.QN.disconnect();
    }

    @Deprecated
    public String getAccountName() {
        return this.QN.getAccountName();
    }

    @Deprecated
    public Person getCurrentPerson() {
        return this.QN.getCurrentPerson();
    }

    e hj() {
        return this.QN;
    }

    @Deprecated
    public boolean isConnected() {
        return this.QN.isConnected();
    }

    @Deprecated
    public boolean isConnecting() {
        return this.QN.isConnecting();
    }

    @Deprecated
    public boolean isConnectionCallbacksRegistered(ConnectionCallbacks listener) {
        return this.QN.isConnectionCallbacksRegistered(listener);
    }

    @Deprecated
    public boolean isConnectionFailedListenerRegistered(OnConnectionFailedListener listener) {
        return this.QN.isConnectionFailedListenerRegistered(listener);
    }

    @Deprecated
    public void loadMoments(OnMomentsLoadedListener listener) {
        this.QN.i(new AnonymousClass1(this, listener));
    }

    @Deprecated
    public void loadMoments(OnMomentsLoadedListener listener, int maxResults, String pageToken, Uri targetUrl, String type, String userId) {
        this.QN.a(new AnonymousClass2(this, listener), maxResults, pageToken, targetUrl, type, userId);
    }

    @Deprecated
    public void loadPeople(OnPeopleLoadedListener listener, Collection<String> personIds) {
        this.QN.a(new AnonymousClass5(this, listener), (Collection) personIds);
    }

    @Deprecated
    public void loadPeople(OnPeopleLoadedListener listener, String... personIds) {
        this.QN.c(new AnonymousClass6(this, listener), personIds);
    }

    @Deprecated
    public void loadVisiblePeople(OnPeopleLoadedListener listener, int orderBy, String pageToken) {
        this.QN.a(new AnonymousClass3(this, listener), orderBy, pageToken);
    }

    @Deprecated
    public void loadVisiblePeople(OnPeopleLoadedListener listener, String pageToken) {
        this.QN.i(new AnonymousClass4(this, listener), pageToken);
    }

    @Deprecated
    public void registerConnectionCallbacks(ConnectionCallbacks listener) {
        this.QN.registerConnectionCallbacks(listener);
    }

    @Deprecated
    public void registerConnectionFailedListener(OnConnectionFailedListener listener) {
        this.QN.registerConnectionFailedListener(listener);
    }

    @Deprecated
    public void removeMoment(String momentId) {
        this.QN.removeMoment(momentId);
    }

    @Deprecated
    public void revokeAccessAndDisconnect(OnAccessRevokedListener listener) {
        this.QN.k(new AnonymousClass7(this, listener));
    }

    @Deprecated
    public void unregisterConnectionCallbacks(ConnectionCallbacks listener) {
        this.QN.unregisterConnectionCallbacks(listener);
    }

    @Deprecated
    public void unregisterConnectionFailedListener(OnConnectionFailedListener listener) {
        this.QN.unregisterConnectionFailedListener(listener);
    }

    @Deprecated
    public void writeMoment(Moment moment) {
        this.QN.a(null, moment);
    }
}
