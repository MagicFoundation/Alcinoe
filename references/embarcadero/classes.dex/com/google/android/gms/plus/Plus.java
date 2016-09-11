package com.google.android.gms.plus;

import android.content.Context;
import android.os.Looper;
import com.google.android.gms.common.Scopes;
import com.google.android.gms.common.api.Api;
import com.google.android.gms.common.api.Api.b;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.GoogleApiClient.ApiOptions;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Scope;
import com.google.android.gms.internal.ee;
import com.google.android.gms.internal.er;
import com.google.android.gms.internal.ii;
import com.google.android.gms.internal.ij;
import com.google.android.gms.internal.ik;
import com.google.android.gms.internal.il;
import com.google.android.gms.plus.internal.PlusCommonExtras;
import com.google.android.gms.plus.internal.e;
import com.google.android.gms.plus.internal.h;
import java.util.HashSet;
import java.util.Set;

public final class Plus {
    public static final Api API;
    public static final Account AccountApi;
    public static final Moments MomentsApi;
    public static final People PeopleApi;
    public static final a QK;
    public static final Scope SCOPE_PLUS_LOGIN;
    public static final Scope SCOPE_PLUS_PROFILE;
    static final b<e> va;

    public static final class PlusOptions implements ApiOptions {
        final String QL;
        final Set<String> QM;

        public static final class Builder {
            String QL;
            final Set<String> QM;

            public Builder() {
                this.QM = new HashSet();
            }

            public Builder addActivityTypes(String... activityTypes) {
                er.b((Object) activityTypes, (Object) "activityTypes may not be null.");
                for (Object add : activityTypes) {
                    this.QM.add(add);
                }
                return this;
            }

            public PlusOptions build() {
                return new PlusOptions();
            }

            public Builder setServerClientId(String clientId) {
                this.QL = clientId;
                return this;
            }
        }

        private PlusOptions() {
            this.QL = null;
            this.QM = new HashSet();
        }

        private PlusOptions(Builder builder) {
            this.QL = builder.QL;
            this.QM = builder.QM;
        }

        public static Builder builder() {
            return new Builder();
        }
    }

    public static abstract class a<R extends Result> extends com.google.android.gms.common.api.a.a<R, e> {
        public a(b<e> bVar) {
            super(bVar);
        }
    }

    static {
        va = new b<e>() {
            public /* synthetic */ com.google.android.gms.common.api.Api.a b(Context context, Looper looper, ee eeVar, ApiOptions apiOptions, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
                return h(context, looper, eeVar, apiOptions, connectionCallbacks, onConnectionFailedListener);
            }

            public int getPriority() {
                return 2;
            }

            public e h(Context context, Looper looper, ee eeVar, ApiOptions apiOptions, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
                PlusOptions plusOptions;
                PlusOptions plusOptions2 = new PlusOptions();
                if (apiOptions != null) {
                    er.b(apiOptions instanceof PlusOptions, (Object) "Must provide valid PlusOptions!");
                    plusOptions = (PlusOptions) apiOptions;
                } else {
                    plusOptions = plusOptions2;
                }
                return new e(context, looper, connectionCallbacks, onConnectionFailedListener, new h(eeVar.dR(), eeVar.dU(), (String[]) plusOptions.QM.toArray(new String[0]), new String[0], context.getPackageName(), context.getPackageName(), null, new PlusCommonExtras()));
            }
        };
        API = new Api(va, new Scope[0]);
        SCOPE_PLUS_LOGIN = new Scope(Scopes.PLUS_LOGIN);
        SCOPE_PLUS_PROFILE = new Scope(Scopes.PLUS_ME);
        MomentsApi = new ik(va);
        PeopleApi = new il(va);
        AccountApi = new ii(va);
        QK = new ij(va);
    }

    private Plus() {
    }

    public static e a(GoogleApiClient googleApiClient, b<e> bVar) {
        boolean z = true;
        er.b(googleApiClient != null, (Object) "GoogleApiClient parameter is required.");
        er.a(googleApiClient.isConnected(), "GoogleApiClient must be connected.");
        e eVar = (e) googleApiClient.a((b) bVar);
        if (eVar == null) {
            z = false;
        }
        er.a(z, "GoogleApiClient is not configured to use the Plus.API Api. Pass this into GoogleApiClient.Builder#addApi() to use this feature.");
        return eVar;
    }
}
