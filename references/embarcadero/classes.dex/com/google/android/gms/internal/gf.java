package com.google.android.gms.internal;

import android.content.Intent;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.common.api.a.c;
import com.google.android.gms.common.data.DataHolder;
import com.google.android.gms.games.Games;
import com.google.android.gms.games.achievement.AchievementBuffer;
import com.google.android.gms.games.achievement.Achievements;
import com.google.android.gms.games.achievement.Achievements.LoadAchievementsResult;
import com.google.android.gms.games.achievement.Achievements.UpdateAchievementResult;

public final class gf implements Achievements {

    private static abstract class a extends com.google.android.gms.games.Games.a<LoadAchievementsResult> {

        /* renamed from: com.google.android.gms.internal.gf.a.1 */
        class AnonymousClass1 implements LoadAchievementsResult {
            final /* synthetic */ a HL;
            final /* synthetic */ Status vb;

            AnonymousClass1(a aVar, Status status) {
                this.HL = aVar;
                this.vb = status;
            }

            public AchievementBuffer getAchievements() {
                return new AchievementBuffer(DataHolder.empty(14));
            }

            public Status getStatus() {
                return this.vb;
            }

            public void release() {
            }
        }

        private a() {
        }

        public /* synthetic */ Result d(Status status) {
            return s(status);
        }

        public LoadAchievementsResult s(Status status) {
            return new AnonymousClass1(this, status);
        }
    }

    private static abstract class b extends com.google.android.gms.games.Games.a<UpdateAchievementResult> {
        private final String uS;

        /* renamed from: com.google.android.gms.internal.gf.b.1 */
        class AnonymousClass1 implements UpdateAchievementResult {
            final /* synthetic */ b HM;
            final /* synthetic */ Status vb;

            AnonymousClass1(b bVar, Status status) {
                this.HM = bVar;
                this.vb = status;
            }

            public String getAchievementId() {
                return this.HM.uS;
            }

            public Status getStatus() {
                return this.vb;
            }
        }

        public b(String str) {
            this.uS = str;
        }

        public /* synthetic */ Result d(Status status) {
            return t(status);
        }

        public UpdateAchievementResult t(Status status) {
            return new AnonymousClass1(this, status);
        }
    }

    /* renamed from: com.google.android.gms.internal.gf.1 */
    class AnonymousClass1 extends a {
        final /* synthetic */ boolean HH;
        final /* synthetic */ gf HI;

        AnonymousClass1(gf gfVar, boolean z) {
            this.HI = gfVar;
            this.HH = z;
            super();
        }

        public void a(fx fxVar) {
            fxVar.c((c) this, this.HH);
        }
    }

    /* renamed from: com.google.android.gms.internal.gf.2 */
    class AnonymousClass2 extends b {
        final /* synthetic */ gf HI;
        final /* synthetic */ String HJ;

        AnonymousClass2(gf gfVar, String str, String str2) {
            this.HI = gfVar;
            this.HJ = str2;
            super(str);
        }

        public void a(fx fxVar) {
            fxVar.b(null, this.HJ);
        }
    }

    /* renamed from: com.google.android.gms.internal.gf.3 */
    class AnonymousClass3 extends b {
        final /* synthetic */ gf HI;
        final /* synthetic */ String HJ;

        AnonymousClass3(gf gfVar, String str, String str2) {
            this.HI = gfVar;
            this.HJ = str2;
            super(str);
        }

        public void a(fx fxVar) {
            fxVar.b((c) this, this.HJ);
        }
    }

    /* renamed from: com.google.android.gms.internal.gf.4 */
    class AnonymousClass4 extends b {
        final /* synthetic */ gf HI;
        final /* synthetic */ String HJ;

        AnonymousClass4(gf gfVar, String str, String str2) {
            this.HI = gfVar;
            this.HJ = str2;
            super(str);
        }

        public void a(fx fxVar) {
            fxVar.c(null, this.HJ);
        }
    }

    /* renamed from: com.google.android.gms.internal.gf.5 */
    class AnonymousClass5 extends b {
        final /* synthetic */ gf HI;
        final /* synthetic */ String HJ;

        AnonymousClass5(gf gfVar, String str, String str2) {
            this.HI = gfVar;
            this.HJ = str2;
            super(str);
        }

        public void a(fx fxVar) {
            fxVar.c((c) this, this.HJ);
        }
    }

    /* renamed from: com.google.android.gms.internal.gf.6 */
    class AnonymousClass6 extends b {
        final /* synthetic */ gf HI;
        final /* synthetic */ String HJ;
        final /* synthetic */ int HK;

        AnonymousClass6(gf gfVar, String str, String str2, int i) {
            this.HI = gfVar;
            this.HJ = str2;
            this.HK = i;
            super(str);
        }

        public void a(fx fxVar) {
            fxVar.a(null, this.HJ, this.HK);
        }
    }

    /* renamed from: com.google.android.gms.internal.gf.7 */
    class AnonymousClass7 extends b {
        final /* synthetic */ gf HI;
        final /* synthetic */ String HJ;
        final /* synthetic */ int HK;

        AnonymousClass7(gf gfVar, String str, String str2, int i) {
            this.HI = gfVar;
            this.HJ = str2;
            this.HK = i;
            super(str);
        }

        public void a(fx fxVar) {
            fxVar.a((c) this, this.HJ, this.HK);
        }
    }

    /* renamed from: com.google.android.gms.internal.gf.8 */
    class AnonymousClass8 extends b {
        final /* synthetic */ gf HI;
        final /* synthetic */ String HJ;
        final /* synthetic */ int HK;

        AnonymousClass8(gf gfVar, String str, String str2, int i) {
            this.HI = gfVar;
            this.HJ = str2;
            this.HK = i;
            super(str);
        }

        public void a(fx fxVar) {
            fxVar.b(null, this.HJ, this.HK);
        }
    }

    /* renamed from: com.google.android.gms.internal.gf.9 */
    class AnonymousClass9 extends b {
        final /* synthetic */ gf HI;
        final /* synthetic */ String HJ;
        final /* synthetic */ int HK;

        AnonymousClass9(gf gfVar, String str, String str2, int i) {
            this.HI = gfVar;
            this.HJ = str2;
            this.HK = i;
            super(str);
        }

        public void a(fx fxVar) {
            fxVar.b((c) this, this.HJ, this.HK);
        }
    }

    public Intent getAchievementsIntent(GoogleApiClient apiClient) {
        return Games.c(apiClient).fs();
    }

    public void increment(GoogleApiClient apiClient, String id, int numSteps) {
        apiClient.b(new AnonymousClass6(this, id, id, numSteps));
    }

    public PendingResult<UpdateAchievementResult> incrementImmediate(GoogleApiClient apiClient, String id, int numSteps) {
        return apiClient.b(new AnonymousClass7(this, id, id, numSteps));
    }

    public PendingResult<LoadAchievementsResult> load(GoogleApiClient apiClient, boolean forceReload) {
        return apiClient.a(new AnonymousClass1(this, forceReload));
    }

    public void reveal(GoogleApiClient apiClient, String id) {
        apiClient.b(new AnonymousClass2(this, id, id));
    }

    public PendingResult<UpdateAchievementResult> revealImmediate(GoogleApiClient apiClient, String id) {
        return apiClient.b(new AnonymousClass3(this, id, id));
    }

    public void setSteps(GoogleApiClient apiClient, String id, int numSteps) {
        apiClient.b(new AnonymousClass8(this, id, id, numSteps));
    }

    public PendingResult<UpdateAchievementResult> setStepsImmediate(GoogleApiClient apiClient, String id, int numSteps) {
        return apiClient.b(new AnonymousClass9(this, id, id, numSteps));
    }

    public void unlock(GoogleApiClient apiClient, String id) {
        apiClient.b(new AnonymousClass4(this, id, id));
    }

    public PendingResult<UpdateAchievementResult> unlockImmediate(GoogleApiClient apiClient, String id) {
        return apiClient.b(new AnonymousClass5(this, id, id));
    }
}
