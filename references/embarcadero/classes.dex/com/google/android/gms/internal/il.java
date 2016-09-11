package com.google.android.gms.internal;

import com.google.android.gms.common.api.Api.b;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.common.api.a.c;
import com.google.android.gms.plus.People;
import com.google.android.gms.plus.People.LoadPeopleResult;
import com.google.android.gms.plus.Plus;
import com.google.android.gms.plus.internal.e;
import com.google.android.gms.plus.model.people.Person;
import com.google.android.gms.plus.model.people.PersonBuffer;
import java.util.Collection;

public final class il implements People {
    private final b<e> Rw;

    private static abstract class a extends com.google.android.gms.plus.Plus.a<LoadPeopleResult> {

        /* renamed from: com.google.android.gms.internal.il.a.1 */
        class AnonymousClass1 implements LoadPeopleResult {
            final /* synthetic */ a RK;
            final /* synthetic */ Status vb;

            AnonymousClass1(a aVar, Status status) {
                this.RK = aVar;
                this.vb = status;
            }

            public String getNextPageToken() {
                return null;
            }

            public PersonBuffer getPersonBuffer() {
                return null;
            }

            public Status getStatus() {
                return this.vb;
            }

            public void release() {
            }
        }

        a(b<e> bVar) {
            super(bVar);
        }

        public LoadPeopleResult N(Status status) {
            return new AnonymousClass1(this, status);
        }

        public /* synthetic */ Result d(Status status) {
            return N(status);
        }
    }

    /* renamed from: com.google.android.gms.internal.il.1 */
    class AnonymousClass1 extends a {
        final /* synthetic */ int RG;
        final /* synthetic */ il RH;
        final /* synthetic */ String Rz;

        AnonymousClass1(il ilVar, b bVar, int i, String str) {
            this.RH = ilVar;
            this.RG = i;
            this.Rz = str;
            super(bVar);
        }

        protected void a(e eVar) {
            eVar.a((c) this, this.RG, this.Rz);
        }
    }

    /* renamed from: com.google.android.gms.internal.il.2 */
    class AnonymousClass2 extends a {
        final /* synthetic */ il RH;
        final /* synthetic */ String Rz;

        AnonymousClass2(il ilVar, b bVar, String str) {
            this.RH = ilVar;
            this.Rz = str;
            super(bVar);
        }

        protected void a(e eVar) {
            eVar.i(this, this.Rz);
        }
    }

    /* renamed from: com.google.android.gms.internal.il.3 */
    class AnonymousClass3 extends a {
        final /* synthetic */ il RH;

        AnonymousClass3(il ilVar, b bVar) {
            this.RH = ilVar;
            super(bVar);
        }

        protected void a(e eVar) {
            eVar.j(this);
        }
    }

    /* renamed from: com.google.android.gms.internal.il.4 */
    class AnonymousClass4 extends a {
        final /* synthetic */ il RH;
        final /* synthetic */ Collection RI;

        AnonymousClass4(il ilVar, b bVar, Collection collection) {
            this.RH = ilVar;
            this.RI = collection;
            super(bVar);
        }

        protected void a(e eVar) {
            eVar.a((c) this, this.RI);
        }
    }

    /* renamed from: com.google.android.gms.internal.il.5 */
    class AnonymousClass5 extends a {
        final /* synthetic */ il RH;
        final /* synthetic */ String[] RJ;

        AnonymousClass5(il ilVar, b bVar, String[] strArr) {
            this.RH = ilVar;
            this.RJ = strArr;
            super(bVar);
        }

        protected void a(e eVar) {
            eVar.c(this, this.RJ);
        }
    }

    public il(b<e> bVar) {
        this.Rw = bVar;
    }

    public Person getCurrentPerson(GoogleApiClient googleApiClient) {
        return Plus.a(googleApiClient, this.Rw).getCurrentPerson();
    }

    public PendingResult<LoadPeopleResult> load(GoogleApiClient googleApiClient, Collection<String> personIds) {
        return googleApiClient.a(new AnonymousClass4(this, this.Rw, personIds));
    }

    public PendingResult<LoadPeopleResult> load(GoogleApiClient googleApiClient, String... personIds) {
        return googleApiClient.a(new AnonymousClass5(this, this.Rw, personIds));
    }

    public PendingResult<LoadPeopleResult> loadConnected(GoogleApiClient googleApiClient) {
        return googleApiClient.a(new AnonymousClass3(this, this.Rw));
    }

    public PendingResult<LoadPeopleResult> loadVisible(GoogleApiClient googleApiClient, int orderBy, String pageToken) {
        return googleApiClient.a(new AnonymousClass1(this, this.Rw, orderBy, pageToken));
    }

    public PendingResult<LoadPeopleResult> loadVisible(GoogleApiClient googleApiClient, String pageToken) {
        return googleApiClient.a(new AnonymousClass2(this, this.Rw, pageToken));
    }
}
