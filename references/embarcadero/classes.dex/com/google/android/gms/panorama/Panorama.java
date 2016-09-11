package com.google.android.gms.panorama;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Looper;
import com.google.android.gms.common.api.Api;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.GoogleApiClient.ApiOptions;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Scope;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.internal.ee;
import com.google.android.gms.internal.ih;

public final class Panorama {
    public static final Api API;
    static final com.google.android.gms.common.api.Api.b<ih> va;

    public interface PanoramaResult extends Result {
        Intent getViewerIntent();
    }

    public interface a extends PanoramaResult {
    }

    private static abstract class b extends com.google.android.gms.common.api.a.a<PanoramaResult, ih> {

        /* renamed from: com.google.android.gms.panorama.Panorama.b.1 */
        class AnonymousClass1 implements PanoramaResult {
            final /* synthetic */ b QA;
            final /* synthetic */ Status vb;

            AnonymousClass1(b bVar, Status status) {
                this.QA = bVar;
                this.vb = status;
            }

            public Status getStatus() {
                return this.vb;
            }

            public Intent getViewerIntent() {
                return null;
            }
        }

        public b() {
            super(Panorama.va);
        }

        public PanoramaResult J(Status status) {
            return new AnonymousClass1(this, status);
        }

        public /* synthetic */ Result d(Status status) {
            return J(status);
        }
    }

    /* renamed from: com.google.android.gms.panorama.Panorama.2 */
    static class AnonymousClass2 extends b {
        final /* synthetic */ Uri Qz;

        AnonymousClass2(Uri uri) {
            this.Qz = uri;
        }

        protected void a(ih ihVar) {
            ihVar.a(this, this.Qz, false);
        }
    }

    /* renamed from: com.google.android.gms.panorama.Panorama.3 */
    static class AnonymousClass3 extends b {
        final /* synthetic */ Uri Qz;

        AnonymousClass3(Uri uri) {
            this.Qz = uri;
        }

        protected void a(ih ihVar) {
            ihVar.a(this, this.Qz, true);
        }
    }

    static {
        va = new com.google.android.gms.common.api.Api.b<ih>() {
            public /* synthetic */ com.google.android.gms.common.api.Api.a b(Context context, Looper looper, ee eeVar, ApiOptions apiOptions, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
                return g(context, looper, eeVar, apiOptions, connectionCallbacks, onConnectionFailedListener);
            }

            public ih g(Context context, Looper looper, ee eeVar, ApiOptions apiOptions, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
                return new ih(context, looper, connectionCallbacks, onConnectionFailedListener);
            }

            public int getPriority() {
                return Integer.MAX_VALUE;
            }
        };
        API = new Api(va, new Scope[0]);
    }

    private Panorama() {
    }

    public static PendingResult<PanoramaResult> loadPanoramaInfo(GoogleApiClient client, Uri uri) {
        return client.a(new AnonymousClass2(uri));
    }

    public static PendingResult<PanoramaResult> loadPanoramaInfoAndGrantAccess(GoogleApiClient client, Uri uri) {
        return client.a(new AnonymousClass3(uri));
    }
}
