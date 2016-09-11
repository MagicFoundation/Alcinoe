package com.google.android.gms.internal;

import android.net.Uri;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.plus.Moments;
import com.google.android.gms.plus.Moments.LoadMomentsResult;
import com.google.android.gms.plus.internal.e;
import com.google.android.gms.plus.model.moments.Moment;
import com.google.android.gms.plus.model.moments.MomentBuffer;

public final class ik implements Moments {
    private final com.google.android.gms.common.api.Api.b<e> Rw;

    private static abstract class a extends com.google.android.gms.plus.Plus.a<LoadMomentsResult> {

        /* renamed from: com.google.android.gms.internal.ik.a.1 */
        class AnonymousClass1 implements LoadMomentsResult {
            final /* synthetic */ a RF;
            final /* synthetic */ Status vb;

            AnonymousClass1(a aVar, Status status) {
                this.RF = aVar;
                this.vb = status;
            }

            public MomentBuffer getMomentBuffer() {
                return null;
            }

            public String getNextPageToken() {
                return null;
            }

            public Status getStatus() {
                return this.vb;
            }

            public String getUpdated() {
                return null;
            }

            public void release() {
            }
        }

        a(com.google.android.gms.common.api.Api.b<e> bVar) {
            super(bVar);
        }

        public LoadMomentsResult M(Status status) {
            return new AnonymousClass1(this, status);
        }

        public /* synthetic */ Result d(Status status) {
            return M(status);
        }
    }

    private static abstract class b extends com.google.android.gms.plus.Plus.a<Status> {
        b(com.google.android.gms.common.api.Api.b<e> bVar) {
            super(bVar);
        }

        public /* synthetic */ Result d(Status status) {
            return f(status);
        }

        public Status f(Status status) {
            return status;
        }
    }

    private static abstract class c extends com.google.android.gms.plus.Plus.a<Status> {
        c(com.google.android.gms.common.api.Api.b<e> bVar) {
            super(bVar);
        }

        public /* synthetic */ Result d(Status status) {
            return f(status);
        }

        public Status f(Status status) {
            return status;
        }
    }

    /* renamed from: com.google.android.gms.internal.ik.1 */
    class AnonymousClass1 extends a {
        final /* synthetic */ ik Ry;

        AnonymousClass1(ik ikVar, com.google.android.gms.common.api.Api.b bVar) {
            this.Ry = ikVar;
            super(bVar);
        }

        protected void a(e eVar) {
            eVar.i(this);
        }
    }

    /* renamed from: com.google.android.gms.internal.ik.2 */
    class AnonymousClass2 extends a {
        final /* synthetic */ int HW;
        final /* synthetic */ Uri RA;
        final /* synthetic */ String RB;
        final /* synthetic */ String RC;
        final /* synthetic */ ik Ry;
        final /* synthetic */ String Rz;

        AnonymousClass2(ik ikVar, com.google.android.gms.common.api.Api.b bVar, int i, String str, Uri uri, String str2, String str3) {
            this.Ry = ikVar;
            this.HW = i;
            this.Rz = str;
            this.RA = uri;
            this.RB = str2;
            this.RC = str3;
            super(bVar);
        }

        protected void a(e eVar) {
            eVar.a(this, this.HW, this.Rz, this.RA, this.RB, this.RC);
        }
    }

    /* renamed from: com.google.android.gms.internal.ik.3 */
    class AnonymousClass3 extends c {
        final /* synthetic */ Moment RD;
        final /* synthetic */ ik Ry;

        AnonymousClass3(ik ikVar, com.google.android.gms.common.api.Api.b bVar, Moment moment) {
            this.Ry = ikVar;
            this.RD = moment;
            super(bVar);
        }

        protected void a(e eVar) {
            eVar.a((com.google.android.gms.common.api.a.c) this, this.RD);
        }
    }

    /* renamed from: com.google.android.gms.internal.ik.4 */
    class AnonymousClass4 extends b {
        final /* synthetic */ String RE;
        final /* synthetic */ ik Ry;

        AnonymousClass4(ik ikVar, com.google.android.gms.common.api.Api.b bVar, String str) {
            this.Ry = ikVar;
            this.RE = str;
            super(bVar);
        }

        protected void a(e eVar) {
            eVar.removeMoment(this.RE);
            a(Status.zQ);
        }
    }

    public ik(com.google.android.gms.common.api.Api.b<e> bVar) {
        this.Rw = bVar;
    }

    public PendingResult<LoadMomentsResult> load(GoogleApiClient googleApiClient) {
        return googleApiClient.a(new AnonymousClass1(this, this.Rw));
    }

    public PendingResult<LoadMomentsResult> load(GoogleApiClient googleApiClient, int maxResults, String pageToken, Uri targetUrl, String type, String userId) {
        return googleApiClient.a(new AnonymousClass2(this, this.Rw, maxResults, pageToken, targetUrl, type, userId));
    }

    public PendingResult<Status> remove(GoogleApiClient googleApiClient, String momentId) {
        return googleApiClient.b(new AnonymousClass4(this, this.Rw, momentId));
    }

    public PendingResult<Status> write(GoogleApiClient googleApiClient, Moment moment) {
        return googleApiClient.b(new AnonymousClass3(this, this.Rw, moment));
    }
}
