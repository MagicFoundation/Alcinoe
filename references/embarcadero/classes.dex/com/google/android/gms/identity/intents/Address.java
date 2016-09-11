package com.google.android.gms.identity.intents;

import android.app.Activity;
import android.content.Context;
import android.os.Looper;
import android.os.RemoteException;
import com.google.android.gms.common.api.Api;
import com.google.android.gms.common.api.Api.b;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.GoogleApiClient.ApiOptions;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Scope;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.internal.ee;
import com.google.android.gms.internal.er;
import com.google.android.gms.internal.hc;

public final class Address {
    public static final Api API;
    static final b<hc> va;

    public static final class AddressOptions implements ApiOptions {
        public final int theme;

        public AddressOptions() {
            this.theme = 0;
        }

        public AddressOptions(int theme) {
            this.theme = theme;
        }
    }

    private static abstract class a extends com.google.android.gms.common.api.a.a<Status, hc> {
        public a() {
            super(Address.va);
        }

        public /* synthetic */ Result d(Status status) {
            return f(status);
        }

        public Status f(Status status) {
            return status;
        }
    }

    /* renamed from: com.google.android.gms.identity.intents.Address.2 */
    static class AnonymousClass2 extends a {
        final /* synthetic */ UserAddressRequest Kw;
        final /* synthetic */ int Kx;

        AnonymousClass2(UserAddressRequest userAddressRequest, int i) {
            this.Kw = userAddressRequest;
            this.Kx = i;
        }

        protected void a(hc hcVar) throws RemoteException {
            hcVar.a(this.Kw, this.Kx);
            a(Status.zQ);
        }
    }

    static {
        va = new b<hc>() {
            public /* synthetic */ com.google.android.gms.common.api.Api.a b(Context context, Looper looper, ee eeVar, ApiOptions apiOptions, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
                return f(context, looper, eeVar, apiOptions, connectionCallbacks, onConnectionFailedListener);
            }

            public hc f(Context context, Looper looper, ee eeVar, ApiOptions apiOptions, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
                AddressOptions addressOptions;
                er.b(context instanceof Activity, (Object) "An Activity must be used for Address APIs");
                AddressOptions addressOptions2 = new AddressOptions();
                if (apiOptions != null) {
                    er.b(apiOptions instanceof AddressOptions, (Object) "Must use AddressOptions with Address API");
                    addressOptions = (AddressOptions) apiOptions;
                } else {
                    addressOptions = addressOptions2;
                }
                return new hc((Activity) context, looper, connectionCallbacks, onConnectionFailedListener, eeVar.getAccountName(), addressOptions.theme);
            }

            public int getPriority() {
                return Integer.MAX_VALUE;
            }
        };
        API = new Api(va, new Scope[0]);
    }

    public static void requestUserAddress(GoogleApiClient googleApiClient, UserAddressRequest request, int requestCode) {
        googleApiClient.a(new AnonymousClass2(request, requestCode));
    }
}
