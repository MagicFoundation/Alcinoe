package com.google.android.gms.internal;

import android.os.Parcel;
import android.support.v4.media.TransportMediator;
import android.support.v4.util.TimeUtils;
import com.google.analytics.tracking.android.ModelFields;
import com.google.android.gms.common.Scopes;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.plus.PlusShare;
import com.google.android.gms.plus.model.people.Person;
import com.google.android.gms.plus.model.people.Person.AgeRange;
import com.google.android.gms.plus.model.people.Person.Cover;
import com.google.android.gms.plus.model.people.Person.Cover.CoverInfo;
import com.google.android.gms.plus.model.people.Person.Cover.CoverPhoto;
import com.google.android.gms.plus.model.people.Person.Image;
import com.google.android.gms.plus.model.people.Person.Name;
import com.google.android.gms.plus.model.people.Person.Organizations;
import com.google.android.gms.plus.model.people.Person.PlacesLived;
import com.google.android.gms.plus.model.people.Person.Urls;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public final class ir extends fb implements SafeParcelable, Person {
    public static final is CREATOR;
    private static final HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> RL;
    private String FE;
    private final Set<Integer> RM;
    private String SK;
    private a SL;
    private String SM;
    private String SN;
    private int SO;
    private b SP;
    private String SQ;
    private c SR;
    private boolean SS;
    private String ST;
    private d SU;
    private String SV;
    private int SW;
    private List<f> SX;
    private List<g> SY;
    private int SZ;
    private int Ta;
    private String Tb;
    private List<h> Tc;
    private boolean Td;
    private int lu;
    private String pS;
    private String uS;
    private final int wj;

    public static class e {
        public static int aT(String str) {
            if (str.equals("person")) {
                return 0;
            }
            if (str.equals(ModelFields.PAGE)) {
                return 1;
            }
            throw new IllegalArgumentException("Unknown objectType string: " + str);
        }
    }

    public static final class a extends fb implements SafeParcelable, AgeRange {
        public static final it CREATOR;
        private static final HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> RL;
        private final Set<Integer> RM;
        private int Te;
        private int Tf;
        private final int wj;

        static {
            CREATOR = new it();
            RL = new HashMap();
            RL.put("max", com.google.android.gms.internal.fb.a.g("max", 2));
            RL.put("min", com.google.android.gms.internal.fb.a.g("min", 3));
        }

        public a() {
            this.wj = 1;
            this.RM = new HashSet();
        }

        a(Set<Integer> set, int i, int i2, int i3) {
            this.RM = set;
            this.wj = i;
            this.Te = i2;
            this.Tf = i3;
        }

        protected boolean a(com.google.android.gms.internal.fb.a aVar) {
            return this.RM.contains(Integer.valueOf(aVar.eu()));
        }

        protected Object ak(String str) {
            return null;
        }

        protected boolean al(String str) {
            return false;
        }

        protected Object b(com.google.android.gms.internal.fb.a aVar) {
            switch (aVar.eu()) {
                case DetectedActivity.ON_FOOT /*2*/:
                    return Integer.valueOf(this.Te);
                case DetectedActivity.STILL /*3*/:
                    return Integer.valueOf(this.Tf);
                default:
                    throw new IllegalStateException("Unknown safe parcelable id=" + aVar.eu());
            }
        }

        public int describeContents() {
            it itVar = CREATOR;
            return 0;
        }

        public HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> en() {
            return RL;
        }

        public boolean equals(Object obj) {
            if (!(obj instanceof a)) {
                return false;
            }
            if (this == obj) {
                return true;
            }
            a aVar = (a) obj;
            for (com.google.android.gms.internal.fb.a aVar2 : RL.values()) {
                if (a(aVar2)) {
                    if (!aVar.a(aVar2)) {
                        return false;
                    }
                    if (!b(aVar2).equals(aVar.b(aVar2))) {
                        return false;
                    }
                } else if (aVar.a(aVar2)) {
                    return false;
                }
            }
            return true;
        }

        public /* synthetic */ Object freeze() {
            return ie();
        }

        public int getMax() {
            return this.Te;
        }

        public int getMin() {
            return this.Tf;
        }

        int getVersionCode() {
            return this.wj;
        }

        Set<Integer> hB() {
            return this.RM;
        }

        public boolean hasMax() {
            return this.RM.contains(Integer.valueOf(2));
        }

        public boolean hasMin() {
            return this.RM.contains(Integer.valueOf(3));
        }

        public int hashCode() {
            int i = 0;
            for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                int hashCode;
                if (a(aVar)) {
                    hashCode = b(aVar).hashCode() + (i + aVar.eu());
                } else {
                    hashCode = i;
                }
                i = hashCode;
            }
            return i;
        }

        public a ie() {
            return this;
        }

        public boolean isDataValid() {
            return true;
        }

        public void writeToParcel(Parcel out, int flags) {
            it itVar = CREATOR;
            it.a(this, out, flags);
        }
    }

    public static final class b extends fb implements SafeParcelable, Cover {
        public static final iu CREATOR;
        private static final HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> RL;
        private final Set<Integer> RM;
        private a Tg;
        private b Th;
        private int Ti;
        private final int wj;

        public static final class a extends fb implements SafeParcelable, CoverInfo {
            public static final iv CREATOR;
            private static final HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> RL;
            private final Set<Integer> RM;
            private int Tj;
            private int Tk;
            private final int wj;

            static {
                CREATOR = new iv();
                RL = new HashMap();
                RL.put("leftImageOffset", com.google.android.gms.internal.fb.a.g("leftImageOffset", 2));
                RL.put("topImageOffset", com.google.android.gms.internal.fb.a.g("topImageOffset", 3));
            }

            public a() {
                this.wj = 1;
                this.RM = new HashSet();
            }

            a(Set<Integer> set, int i, int i2, int i3) {
                this.RM = set;
                this.wj = i;
                this.Tj = i2;
                this.Tk = i3;
            }

            protected boolean a(com.google.android.gms.internal.fb.a aVar) {
                return this.RM.contains(Integer.valueOf(aVar.eu()));
            }

            protected Object ak(String str) {
                return null;
            }

            protected boolean al(String str) {
                return false;
            }

            protected Object b(com.google.android.gms.internal.fb.a aVar) {
                switch (aVar.eu()) {
                    case DetectedActivity.ON_FOOT /*2*/:
                        return Integer.valueOf(this.Tj);
                    case DetectedActivity.STILL /*3*/:
                        return Integer.valueOf(this.Tk);
                    default:
                        throw new IllegalStateException("Unknown safe parcelable id=" + aVar.eu());
                }
            }

            public int describeContents() {
                iv ivVar = CREATOR;
                return 0;
            }

            public HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> en() {
                return RL;
            }

            public boolean equals(Object obj) {
                if (!(obj instanceof a)) {
                    return false;
                }
                if (this == obj) {
                    return true;
                }
                a aVar = (a) obj;
                for (com.google.android.gms.internal.fb.a aVar2 : RL.values()) {
                    if (a(aVar2)) {
                        if (!aVar.a(aVar2)) {
                            return false;
                        }
                        if (!b(aVar2).equals(aVar.b(aVar2))) {
                            return false;
                        }
                    } else if (aVar.a(aVar2)) {
                        return false;
                    }
                }
                return true;
            }

            public /* synthetic */ Object freeze() {
                return ii();
            }

            public int getLeftImageOffset() {
                return this.Tj;
            }

            public int getTopImageOffset() {
                return this.Tk;
            }

            int getVersionCode() {
                return this.wj;
            }

            Set<Integer> hB() {
                return this.RM;
            }

            public boolean hasLeftImageOffset() {
                return this.RM.contains(Integer.valueOf(2));
            }

            public boolean hasTopImageOffset() {
                return this.RM.contains(Integer.valueOf(3));
            }

            public int hashCode() {
                int i = 0;
                for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                    int hashCode;
                    if (a(aVar)) {
                        hashCode = b(aVar).hashCode() + (i + aVar.eu());
                    } else {
                        hashCode = i;
                    }
                    i = hashCode;
                }
                return i;
            }

            public a ii() {
                return this;
            }

            public boolean isDataValid() {
                return true;
            }

            public void writeToParcel(Parcel out, int flags) {
                iv ivVar = CREATOR;
                iv.a(this, out, flags);
            }
        }

        public static final class b extends fb implements SafeParcelable, CoverPhoto {
            public static final iw CREATOR;
            private static final HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> RL;
            private final Set<Integer> RM;
            private String pS;
            private int v;
            private int w;
            private final int wj;

            static {
                CREATOR = new iw();
                RL = new HashMap();
                RL.put("height", com.google.android.gms.internal.fb.a.g("height", 2));
                RL.put(PlusShare.KEY_CALL_TO_ACTION_URL, com.google.android.gms.internal.fb.a.j(PlusShare.KEY_CALL_TO_ACTION_URL, 3));
                RL.put("width", com.google.android.gms.internal.fb.a.g("width", 4));
            }

            public b() {
                this.wj = 1;
                this.RM = new HashSet();
            }

            b(Set<Integer> set, int i, int i2, String str, int i3) {
                this.RM = set;
                this.wj = i;
                this.v = i2;
                this.pS = str;
                this.w = i3;
            }

            protected boolean a(com.google.android.gms.internal.fb.a aVar) {
                return this.RM.contains(Integer.valueOf(aVar.eu()));
            }

            protected Object ak(String str) {
                return null;
            }

            protected boolean al(String str) {
                return false;
            }

            protected Object b(com.google.android.gms.internal.fb.a aVar) {
                switch (aVar.eu()) {
                    case DetectedActivity.ON_FOOT /*2*/:
                        return Integer.valueOf(this.v);
                    case DetectedActivity.STILL /*3*/:
                        return this.pS;
                    case DetectedActivity.UNKNOWN /*4*/:
                        return Integer.valueOf(this.w);
                    default:
                        throw new IllegalStateException("Unknown safe parcelable id=" + aVar.eu());
                }
            }

            public int describeContents() {
                iw iwVar = CREATOR;
                return 0;
            }

            public HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> en() {
                return RL;
            }

            public boolean equals(Object obj) {
                if (!(obj instanceof b)) {
                    return false;
                }
                if (this == obj) {
                    return true;
                }
                b bVar = (b) obj;
                for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                    if (a(aVar)) {
                        if (!bVar.a(aVar)) {
                            return false;
                        }
                        if (!b(aVar).equals(bVar.b(aVar))) {
                            return false;
                        }
                    } else if (bVar.a(aVar)) {
                        return false;
                    }
                }
                return true;
            }

            public /* synthetic */ Object freeze() {
                return ij();
            }

            public int getHeight() {
                return this.v;
            }

            public String getUrl() {
                return this.pS;
            }

            int getVersionCode() {
                return this.wj;
            }

            public int getWidth() {
                return this.w;
            }

            Set<Integer> hB() {
                return this.RM;
            }

            public boolean hasHeight() {
                return this.RM.contains(Integer.valueOf(2));
            }

            public boolean hasUrl() {
                return this.RM.contains(Integer.valueOf(3));
            }

            public boolean hasWidth() {
                return this.RM.contains(Integer.valueOf(4));
            }

            public int hashCode() {
                int i = 0;
                for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                    int hashCode;
                    if (a(aVar)) {
                        hashCode = b(aVar).hashCode() + (i + aVar.eu());
                    } else {
                        hashCode = i;
                    }
                    i = hashCode;
                }
                return i;
            }

            public b ij() {
                return this;
            }

            public boolean isDataValid() {
                return true;
            }

            public void writeToParcel(Parcel out, int flags) {
                iw iwVar = CREATOR;
                iw.a(this, out, flags);
            }
        }

        static {
            CREATOR = new iu();
            RL = new HashMap();
            RL.put("coverInfo", com.google.android.gms.internal.fb.a.a("coverInfo", 2, a.class));
            RL.put("coverPhoto", com.google.android.gms.internal.fb.a.a("coverPhoto", 3, b.class));
            RL.put("layout", com.google.android.gms.internal.fb.a.a("layout", 4, new ey().f("banner", 0), false));
        }

        public b() {
            this.wj = 1;
            this.RM = new HashSet();
        }

        b(Set<Integer> set, int i, a aVar, b bVar, int i2) {
            this.RM = set;
            this.wj = i;
            this.Tg = aVar;
            this.Th = bVar;
            this.Ti = i2;
        }

        protected boolean a(com.google.android.gms.internal.fb.a aVar) {
            return this.RM.contains(Integer.valueOf(aVar.eu()));
        }

        protected Object ak(String str) {
            return null;
        }

        protected boolean al(String str) {
            return false;
        }

        protected Object b(com.google.android.gms.internal.fb.a aVar) {
            switch (aVar.eu()) {
                case DetectedActivity.ON_FOOT /*2*/:
                    return this.Tg;
                case DetectedActivity.STILL /*3*/:
                    return this.Th;
                case DetectedActivity.UNKNOWN /*4*/:
                    return Integer.valueOf(this.Ti);
                default:
                    throw new IllegalStateException("Unknown safe parcelable id=" + aVar.eu());
            }
        }

        public int describeContents() {
            iu iuVar = CREATOR;
            return 0;
        }

        public HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> en() {
            return RL;
        }

        public boolean equals(Object obj) {
            if (!(obj instanceof b)) {
                return false;
            }
            if (this == obj) {
                return true;
            }
            b bVar = (b) obj;
            for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                if (a(aVar)) {
                    if (!bVar.a(aVar)) {
                        return false;
                    }
                    if (!b(aVar).equals(bVar.b(aVar))) {
                        return false;
                    }
                } else if (bVar.a(aVar)) {
                    return false;
                }
            }
            return true;
        }

        public /* synthetic */ Object freeze() {
            return ih();
        }

        public CoverInfo getCoverInfo() {
            return this.Tg;
        }

        public CoverPhoto getCoverPhoto() {
            return this.Th;
        }

        public int getLayout() {
            return this.Ti;
        }

        int getVersionCode() {
            return this.wj;
        }

        Set<Integer> hB() {
            return this.RM;
        }

        public boolean hasCoverInfo() {
            return this.RM.contains(Integer.valueOf(2));
        }

        public boolean hasCoverPhoto() {
            return this.RM.contains(Integer.valueOf(3));
        }

        public boolean hasLayout() {
            return this.RM.contains(Integer.valueOf(4));
        }

        public int hashCode() {
            int i = 0;
            for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                int hashCode;
                if (a(aVar)) {
                    hashCode = b(aVar).hashCode() + (i + aVar.eu());
                } else {
                    hashCode = i;
                }
                i = hashCode;
            }
            return i;
        }

        a if() {
            return this.Tg;
        }

        b ig() {
            return this.Th;
        }

        public b ih() {
            return this;
        }

        public boolean isDataValid() {
            return true;
        }

        public void writeToParcel(Parcel out, int flags) {
            iu iuVar = CREATOR;
            iu.a(this, out, flags);
        }
    }

    public static final class c extends fb implements SafeParcelable, Image {
        public static final ix CREATOR;
        private static final HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> RL;
        private final Set<Integer> RM;
        private String pS;
        private final int wj;

        static {
            CREATOR = new ix();
            RL = new HashMap();
            RL.put(PlusShare.KEY_CALL_TO_ACTION_URL, com.google.android.gms.internal.fb.a.j(PlusShare.KEY_CALL_TO_ACTION_URL, 2));
        }

        public c() {
            this.wj = 1;
            this.RM = new HashSet();
        }

        public c(String str) {
            this.RM = new HashSet();
            this.wj = 1;
            this.pS = str;
            this.RM.add(Integer.valueOf(2));
        }

        c(Set<Integer> set, int i, String str) {
            this.RM = set;
            this.wj = i;
            this.pS = str;
        }

        protected boolean a(com.google.android.gms.internal.fb.a aVar) {
            return this.RM.contains(Integer.valueOf(aVar.eu()));
        }

        protected Object ak(String str) {
            return null;
        }

        protected boolean al(String str) {
            return false;
        }

        protected Object b(com.google.android.gms.internal.fb.a aVar) {
            switch (aVar.eu()) {
                case DetectedActivity.ON_FOOT /*2*/:
                    return this.pS;
                default:
                    throw new IllegalStateException("Unknown safe parcelable id=" + aVar.eu());
            }
        }

        public int describeContents() {
            ix ixVar = CREATOR;
            return 0;
        }

        public HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> en() {
            return RL;
        }

        public boolean equals(Object obj) {
            if (!(obj instanceof c)) {
                return false;
            }
            if (this == obj) {
                return true;
            }
            c cVar = (c) obj;
            for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                if (a(aVar)) {
                    if (!cVar.a(aVar)) {
                        return false;
                    }
                    if (!b(aVar).equals(cVar.b(aVar))) {
                        return false;
                    }
                } else if (cVar.a(aVar)) {
                    return false;
                }
            }
            return true;
        }

        public /* synthetic */ Object freeze() {
            return ik();
        }

        public String getUrl() {
            return this.pS;
        }

        int getVersionCode() {
            return this.wj;
        }

        Set<Integer> hB() {
            return this.RM;
        }

        public boolean hasUrl() {
            return this.RM.contains(Integer.valueOf(2));
        }

        public int hashCode() {
            int i = 0;
            for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                int hashCode;
                if (a(aVar)) {
                    hashCode = b(aVar).hashCode() + (i + aVar.eu());
                } else {
                    hashCode = i;
                }
                i = hashCode;
            }
            return i;
        }

        public c ik() {
            return this;
        }

        public boolean isDataValid() {
            return true;
        }

        public void writeToParcel(Parcel out, int flags) {
            ix ixVar = CREATOR;
            ix.a(this, out, flags);
        }
    }

    public static final class d extends fb implements SafeParcelable, Name {
        public static final iy CREATOR;
        private static final HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> RL;
        private final Set<Integer> RM;
        private String Sk;
        private String Sn;
        private String Tl;
        private String Tm;
        private String Tn;
        private String To;
        private final int wj;

        static {
            CREATOR = new iy();
            RL = new HashMap();
            RL.put("familyName", com.google.android.gms.internal.fb.a.j("familyName", 2));
            RL.put("formatted", com.google.android.gms.internal.fb.a.j("formatted", 3));
            RL.put("givenName", com.google.android.gms.internal.fb.a.j("givenName", 4));
            RL.put("honorificPrefix", com.google.android.gms.internal.fb.a.j("honorificPrefix", 5));
            RL.put("honorificSuffix", com.google.android.gms.internal.fb.a.j("honorificSuffix", 6));
            RL.put("middleName", com.google.android.gms.internal.fb.a.j("middleName", 7));
        }

        public d() {
            this.wj = 1;
            this.RM = new HashSet();
        }

        d(Set<Integer> set, int i, String str, String str2, String str3, String str4, String str5, String str6) {
            this.RM = set;
            this.wj = i;
            this.Sk = str;
            this.Tl = str2;
            this.Sn = str3;
            this.Tm = str4;
            this.Tn = str5;
            this.To = str6;
        }

        protected boolean a(com.google.android.gms.internal.fb.a aVar) {
            return this.RM.contains(Integer.valueOf(aVar.eu()));
        }

        protected Object ak(String str) {
            return null;
        }

        protected boolean al(String str) {
            return false;
        }

        protected Object b(com.google.android.gms.internal.fb.a aVar) {
            switch (aVar.eu()) {
                case DetectedActivity.ON_FOOT /*2*/:
                    return this.Sk;
                case DetectedActivity.STILL /*3*/:
                    return this.Tl;
                case DetectedActivity.UNKNOWN /*4*/:
                    return this.Sn;
                case DetectedActivity.TILTING /*5*/:
                    return this.Tm;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    return this.Tn;
                case Error.AVS_DECLINE /*7*/:
                    return this.To;
                default:
                    throw new IllegalStateException("Unknown safe parcelable id=" + aVar.eu());
            }
        }

        public int describeContents() {
            iy iyVar = CREATOR;
            return 0;
        }

        public HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> en() {
            return RL;
        }

        public boolean equals(Object obj) {
            if (!(obj instanceof d)) {
                return false;
            }
            if (this == obj) {
                return true;
            }
            d dVar = (d) obj;
            for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                if (a(aVar)) {
                    if (!dVar.a(aVar)) {
                        return false;
                    }
                    if (!b(aVar).equals(dVar.b(aVar))) {
                        return false;
                    }
                } else if (dVar.a(aVar)) {
                    return false;
                }
            }
            return true;
        }

        public /* synthetic */ Object freeze() {
            return il();
        }

        public String getFamilyName() {
            return this.Sk;
        }

        public String getFormatted() {
            return this.Tl;
        }

        public String getGivenName() {
            return this.Sn;
        }

        public String getHonorificPrefix() {
            return this.Tm;
        }

        public String getHonorificSuffix() {
            return this.Tn;
        }

        public String getMiddleName() {
            return this.To;
        }

        int getVersionCode() {
            return this.wj;
        }

        Set<Integer> hB() {
            return this.RM;
        }

        public boolean hasFamilyName() {
            return this.RM.contains(Integer.valueOf(2));
        }

        public boolean hasFormatted() {
            return this.RM.contains(Integer.valueOf(3));
        }

        public boolean hasGivenName() {
            return this.RM.contains(Integer.valueOf(4));
        }

        public boolean hasHonorificPrefix() {
            return this.RM.contains(Integer.valueOf(5));
        }

        public boolean hasHonorificSuffix() {
            return this.RM.contains(Integer.valueOf(6));
        }

        public boolean hasMiddleName() {
            return this.RM.contains(Integer.valueOf(7));
        }

        public int hashCode() {
            int i = 0;
            for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                int hashCode;
                if (a(aVar)) {
                    hashCode = b(aVar).hashCode() + (i + aVar.eu());
                } else {
                    hashCode = i;
                }
                i = hashCode;
            }
            return i;
        }

        public d il() {
            return this;
        }

        public boolean isDataValid() {
            return true;
        }

        public void writeToParcel(Parcel out, int flags) {
            iy iyVar = CREATOR;
            iy.a(this, out, flags);
        }
    }

    public static final class f extends fb implements SafeParcelable, Organizations {
        public static final iz CREATOR;
        private static final HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> RL;
        private int AI;
        private String CX;
        private String FH;
        private final Set<Integer> RM;
        private String Sj;
        private String Sz;
        private String Tp;
        private String Tq;
        private boolean Tr;
        private String mName;
        private final int wj;

        static {
            CREATOR = new iz();
            RL = new HashMap();
            RL.put("department", com.google.android.gms.internal.fb.a.j("department", 2));
            RL.put(PlusShare.KEY_CONTENT_DEEP_LINK_METADATA_DESCRIPTION, com.google.android.gms.internal.fb.a.j(PlusShare.KEY_CONTENT_DEEP_LINK_METADATA_DESCRIPTION, 3));
            RL.put("endDate", com.google.android.gms.internal.fb.a.j("endDate", 4));
            RL.put("location", com.google.android.gms.internal.fb.a.j("location", 5));
            RL.put("name", com.google.android.gms.internal.fb.a.j("name", 6));
            RL.put("primary", com.google.android.gms.internal.fb.a.i("primary", 7));
            RL.put("startDate", com.google.android.gms.internal.fb.a.j("startDate", 8));
            RL.put(PlusShare.KEY_CONTENT_DEEP_LINK_METADATA_TITLE, com.google.android.gms.internal.fb.a.j(PlusShare.KEY_CONTENT_DEEP_LINK_METADATA_TITLE, 9));
            RL.put("type", com.google.android.gms.internal.fb.a.a("type", 10, new ey().f("work", 0).f("school", 1), false));
        }

        public f() {
            this.wj = 1;
            this.RM = new HashSet();
        }

        f(Set<Integer> set, int i, String str, String str2, String str3, String str4, String str5, boolean z, String str6, String str7, int i2) {
            this.RM = set;
            this.wj = i;
            this.Tp = str;
            this.FH = str2;
            this.Sj = str3;
            this.Tq = str4;
            this.mName = str5;
            this.Tr = z;
            this.Sz = str6;
            this.CX = str7;
            this.AI = i2;
        }

        protected boolean a(com.google.android.gms.internal.fb.a aVar) {
            return this.RM.contains(Integer.valueOf(aVar.eu()));
        }

        protected Object ak(String str) {
            return null;
        }

        protected boolean al(String str) {
            return false;
        }

        protected Object b(com.google.android.gms.internal.fb.a aVar) {
            switch (aVar.eu()) {
                case DetectedActivity.ON_FOOT /*2*/:
                    return this.Tp;
                case DetectedActivity.STILL /*3*/:
                    return this.FH;
                case DetectedActivity.UNKNOWN /*4*/:
                    return this.Sj;
                case DetectedActivity.TILTING /*5*/:
                    return this.Tq;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    return this.mName;
                case Error.AVS_DECLINE /*7*/:
                    return Boolean.valueOf(this.Tr);
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    return this.Sz;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    return this.CX;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    return Integer.valueOf(this.AI);
                default:
                    throw new IllegalStateException("Unknown safe parcelable id=" + aVar.eu());
            }
        }

        public int describeContents() {
            iz izVar = CREATOR;
            return 0;
        }

        public HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> en() {
            return RL;
        }

        public boolean equals(Object obj) {
            if (!(obj instanceof f)) {
                return false;
            }
            if (this == obj) {
                return true;
            }
            f fVar = (f) obj;
            for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                if (a(aVar)) {
                    if (!fVar.a(aVar)) {
                        return false;
                    }
                    if (!b(aVar).equals(fVar.b(aVar))) {
                        return false;
                    }
                } else if (fVar.a(aVar)) {
                    return false;
                }
            }
            return true;
        }

        public /* synthetic */ Object freeze() {
            return im();
        }

        public String getDepartment() {
            return this.Tp;
        }

        public String getDescription() {
            return this.FH;
        }

        public String getEndDate() {
            return this.Sj;
        }

        public String getLocation() {
            return this.Tq;
        }

        public String getName() {
            return this.mName;
        }

        public String getStartDate() {
            return this.Sz;
        }

        public String getTitle() {
            return this.CX;
        }

        public int getType() {
            return this.AI;
        }

        int getVersionCode() {
            return this.wj;
        }

        Set<Integer> hB() {
            return this.RM;
        }

        public boolean hasDepartment() {
            return this.RM.contains(Integer.valueOf(2));
        }

        public boolean hasDescription() {
            return this.RM.contains(Integer.valueOf(3));
        }

        public boolean hasEndDate() {
            return this.RM.contains(Integer.valueOf(4));
        }

        public boolean hasLocation() {
            return this.RM.contains(Integer.valueOf(5));
        }

        public boolean hasName() {
            return this.RM.contains(Integer.valueOf(6));
        }

        public boolean hasPrimary() {
            return this.RM.contains(Integer.valueOf(7));
        }

        public boolean hasStartDate() {
            return this.RM.contains(Integer.valueOf(8));
        }

        public boolean hasTitle() {
            return this.RM.contains(Integer.valueOf(9));
        }

        public boolean hasType() {
            return this.RM.contains(Integer.valueOf(10));
        }

        public int hashCode() {
            int i = 0;
            for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                int hashCode;
                if (a(aVar)) {
                    hashCode = b(aVar).hashCode() + (i + aVar.eu());
                } else {
                    hashCode = i;
                }
                i = hashCode;
            }
            return i;
        }

        public f im() {
            return this;
        }

        public boolean isDataValid() {
            return true;
        }

        public boolean isPrimary() {
            return this.Tr;
        }

        public void writeToParcel(Parcel out, int flags) {
            iz izVar = CREATOR;
            iz.a(this, out, flags);
        }
    }

    public static final class g extends fb implements SafeParcelable, PlacesLived {
        public static final ja CREATOR;
        private static final HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> RL;
        private final Set<Integer> RM;
        private boolean Tr;
        private String mValue;
        private final int wj;

        static {
            CREATOR = new ja();
            RL = new HashMap();
            RL.put("primary", com.google.android.gms.internal.fb.a.i("primary", 2));
            RL.put("value", com.google.android.gms.internal.fb.a.j("value", 3));
        }

        public g() {
            this.wj = 1;
            this.RM = new HashSet();
        }

        g(Set<Integer> set, int i, boolean z, String str) {
            this.RM = set;
            this.wj = i;
            this.Tr = z;
            this.mValue = str;
        }

        protected boolean a(com.google.android.gms.internal.fb.a aVar) {
            return this.RM.contains(Integer.valueOf(aVar.eu()));
        }

        protected Object ak(String str) {
            return null;
        }

        protected boolean al(String str) {
            return false;
        }

        protected Object b(com.google.android.gms.internal.fb.a aVar) {
            switch (aVar.eu()) {
                case DetectedActivity.ON_FOOT /*2*/:
                    return Boolean.valueOf(this.Tr);
                case DetectedActivity.STILL /*3*/:
                    return this.mValue;
                default:
                    throw new IllegalStateException("Unknown safe parcelable id=" + aVar.eu());
            }
        }

        public int describeContents() {
            ja jaVar = CREATOR;
            return 0;
        }

        public HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> en() {
            return RL;
        }

        public boolean equals(Object obj) {
            if (!(obj instanceof g)) {
                return false;
            }
            if (this == obj) {
                return true;
            }
            g gVar = (g) obj;
            for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                if (a(aVar)) {
                    if (!gVar.a(aVar)) {
                        return false;
                    }
                    if (!b(aVar).equals(gVar.b(aVar))) {
                        return false;
                    }
                } else if (gVar.a(aVar)) {
                    return false;
                }
            }
            return true;
        }

        public /* synthetic */ Object freeze() {
            return in();
        }

        public String getValue() {
            return this.mValue;
        }

        int getVersionCode() {
            return this.wj;
        }

        Set<Integer> hB() {
            return this.RM;
        }

        public boolean hasPrimary() {
            return this.RM.contains(Integer.valueOf(2));
        }

        public boolean hasValue() {
            return this.RM.contains(Integer.valueOf(3));
        }

        public int hashCode() {
            int i = 0;
            for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                int hashCode;
                if (a(aVar)) {
                    hashCode = b(aVar).hashCode() + (i + aVar.eu());
                } else {
                    hashCode = i;
                }
                i = hashCode;
            }
            return i;
        }

        public g in() {
            return this;
        }

        public boolean isDataValid() {
            return true;
        }

        public boolean isPrimary() {
            return this.Tr;
        }

        public void writeToParcel(Parcel out, int flags) {
            ja jaVar = CREATOR;
            ja.a(this, out, flags);
        }
    }

    public static final class h extends fb implements SafeParcelable, Urls {
        public static final jb CREATOR;
        private static final HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> RL;
        private int AI;
        private final Set<Integer> RM;
        private String Ts;
        private final int Tt;
        private String mValue;
        private final int wj;

        static {
            CREATOR = new jb();
            RL = new HashMap();
            RL.put(PlusShare.KEY_CALL_TO_ACTION_LABEL, com.google.android.gms.internal.fb.a.j(PlusShare.KEY_CALL_TO_ACTION_LABEL, 5));
            RL.put("type", com.google.android.gms.internal.fb.a.a("type", 6, new ey().f("home", 0).f("work", 1).f("blog", 2).f(Scopes.PROFILE, 3).f("other", 4).f("otherProfile", 5).f("contributor", 6).f("website", 7), false));
            RL.put("value", com.google.android.gms.internal.fb.a.j("value", 4));
        }

        public h() {
            this.Tt = 4;
            this.wj = 2;
            this.RM = new HashSet();
        }

        h(Set<Integer> set, int i, String str, int i2, String str2, int i3) {
            this.Tt = 4;
            this.RM = set;
            this.wj = i;
            this.Ts = str;
            this.AI = i2;
            this.mValue = str2;
        }

        protected boolean a(com.google.android.gms.internal.fb.a aVar) {
            return this.RM.contains(Integer.valueOf(aVar.eu()));
        }

        protected Object ak(String str) {
            return null;
        }

        protected boolean al(String str) {
            return false;
        }

        protected Object b(com.google.android.gms.internal.fb.a aVar) {
            switch (aVar.eu()) {
                case DetectedActivity.UNKNOWN /*4*/:
                    return this.mValue;
                case DetectedActivity.TILTING /*5*/:
                    return this.Ts;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    return Integer.valueOf(this.AI);
                default:
                    throw new IllegalStateException("Unknown safe parcelable id=" + aVar.eu());
            }
        }

        public int describeContents() {
            jb jbVar = CREATOR;
            return 0;
        }

        public HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> en() {
            return RL;
        }

        public boolean equals(Object obj) {
            if (!(obj instanceof h)) {
                return false;
            }
            if (this == obj) {
                return true;
            }
            h hVar = (h) obj;
            for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                if (a(aVar)) {
                    if (!hVar.a(aVar)) {
                        return false;
                    }
                    if (!b(aVar).equals(hVar.b(aVar))) {
                        return false;
                    }
                } else if (hVar.a(aVar)) {
                    return false;
                }
            }
            return true;
        }

        public /* synthetic */ Object freeze() {
            return ip();
        }

        public String getLabel() {
            return this.Ts;
        }

        public int getType() {
            return this.AI;
        }

        public String getValue() {
            return this.mValue;
        }

        int getVersionCode() {
            return this.wj;
        }

        Set<Integer> hB() {
            return this.RM;
        }

        public boolean hasLabel() {
            return this.RM.contains(Integer.valueOf(5));
        }

        public boolean hasType() {
            return this.RM.contains(Integer.valueOf(6));
        }

        public boolean hasValue() {
            return this.RM.contains(Integer.valueOf(4));
        }

        public int hashCode() {
            int i = 0;
            for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
                int hashCode;
                if (a(aVar)) {
                    hashCode = b(aVar).hashCode() + (i + aVar.eu());
                } else {
                    hashCode = i;
                }
                i = hashCode;
            }
            return i;
        }

        @Deprecated
        public int io() {
            return 4;
        }

        public h ip() {
            return this;
        }

        public boolean isDataValid() {
            return true;
        }

        public void writeToParcel(Parcel out, int flags) {
            jb jbVar = CREATOR;
            jb.a(this, out, flags);
        }
    }

    static {
        CREATOR = new is();
        RL = new HashMap();
        RL.put("aboutMe", com.google.android.gms.internal.fb.a.j("aboutMe", 2));
        RL.put("ageRange", com.google.android.gms.internal.fb.a.a("ageRange", 3, a.class));
        RL.put("birthday", com.google.android.gms.internal.fb.a.j("birthday", 4));
        RL.put("braggingRights", com.google.android.gms.internal.fb.a.j("braggingRights", 5));
        RL.put("circledByCount", com.google.android.gms.internal.fb.a.g("circledByCount", 6));
        RL.put("cover", com.google.android.gms.internal.fb.a.a("cover", 7, b.class));
        RL.put("currentLocation", com.google.android.gms.internal.fb.a.j("currentLocation", 8));
        RL.put("displayName", com.google.android.gms.internal.fb.a.j("displayName", 9));
        RL.put("gender", com.google.android.gms.internal.fb.a.a("gender", 12, new ey().f("male", 0).f("female", 1).f("other", 2), false));
        RL.put("id", com.google.android.gms.internal.fb.a.j("id", 14));
        RL.put("image", com.google.android.gms.internal.fb.a.a("image", 15, c.class));
        RL.put("isPlusUser", com.google.android.gms.internal.fb.a.i("isPlusUser", 16));
        RL.put(ModelFields.LANGUAGE, com.google.android.gms.internal.fb.a.j(ModelFields.LANGUAGE, 18));
        RL.put("name", com.google.android.gms.internal.fb.a.a("name", 19, d.class));
        RL.put("nickname", com.google.android.gms.internal.fb.a.j("nickname", 20));
        RL.put("objectType", com.google.android.gms.internal.fb.a.a("objectType", 21, new ey().f("person", 0).f(ModelFields.PAGE, 1), false));
        RL.put("organizations", com.google.android.gms.internal.fb.a.b("organizations", 22, f.class));
        RL.put("placesLived", com.google.android.gms.internal.fb.a.b("placesLived", 23, g.class));
        RL.put("plusOneCount", com.google.android.gms.internal.fb.a.g("plusOneCount", 24));
        RL.put("relationshipStatus", com.google.android.gms.internal.fb.a.a("relationshipStatus", 25, new ey().f("single", 0).f("in_a_relationship", 1).f("engaged", 2).f("married", 3).f("its_complicated", 4).f("open_relationship", 5).f("widowed", 6).f("in_domestic_partnership", 7).f("in_civil_union", 8), false));
        RL.put("tagline", com.google.android.gms.internal.fb.a.j("tagline", 26));
        RL.put(PlusShare.KEY_CALL_TO_ACTION_URL, com.google.android.gms.internal.fb.a.j(PlusShare.KEY_CALL_TO_ACTION_URL, 27));
        RL.put("urls", com.google.android.gms.internal.fb.a.b("urls", 28, h.class));
        RL.put("verified", com.google.android.gms.internal.fb.a.i("verified", 29));
    }

    public ir() {
        this.wj = 2;
        this.RM = new HashSet();
    }

    public ir(String str, String str2, c cVar, int i, String str3) {
        this.wj = 2;
        this.RM = new HashSet();
        this.FE = str;
        this.RM.add(Integer.valueOf(9));
        this.uS = str2;
        this.RM.add(Integer.valueOf(14));
        this.SR = cVar;
        this.RM.add(Integer.valueOf(15));
        this.SW = i;
        this.RM.add(Integer.valueOf(21));
        this.pS = str3;
        this.RM.add(Integer.valueOf(27));
    }

    ir(Set<Integer> set, int i, String str, a aVar, String str2, String str3, int i2, b bVar, String str4, String str5, int i3, String str6, c cVar, boolean z, String str7, d dVar, String str8, int i4, List<f> list, List<g> list2, int i5, int i6, String str9, String str10, List<h> list3, boolean z2) {
        this.RM = set;
        this.wj = i;
        this.SK = str;
        this.SL = aVar;
        this.SM = str2;
        this.SN = str3;
        this.SO = i2;
        this.SP = bVar;
        this.SQ = str4;
        this.FE = str5;
        this.lu = i3;
        this.uS = str6;
        this.SR = cVar;
        this.SS = z;
        this.ST = str7;
        this.SU = dVar;
        this.SV = str8;
        this.SW = i4;
        this.SX = list;
        this.SY = list2;
        this.SZ = i5;
        this.Ta = i6;
        this.Tb = str9;
        this.pS = str10;
        this.Tc = list3;
        this.Td = z2;
    }

    public static ir i(byte[] bArr) {
        Parcel obtain = Parcel.obtain();
        obtain.unmarshall(bArr, 0, bArr.length);
        obtain.setDataPosition(0);
        ir aI = CREATOR.aI(obtain);
        obtain.recycle();
        return aI;
    }

    protected boolean a(com.google.android.gms.internal.fb.a aVar) {
        return this.RM.contains(Integer.valueOf(aVar.eu()));
    }

    protected Object ak(String str) {
        return null;
    }

    protected boolean al(String str) {
        return false;
    }

    protected Object b(com.google.android.gms.internal.fb.a aVar) {
        switch (aVar.eu()) {
            case DetectedActivity.ON_FOOT /*2*/:
                return this.SK;
            case DetectedActivity.STILL /*3*/:
                return this.SL;
            case DetectedActivity.UNKNOWN /*4*/:
                return this.SM;
            case DetectedActivity.TILTING /*5*/:
                return this.SN;
            case Participant.STATUS_UNRESPONSIVE /*6*/:
                return Integer.valueOf(this.SO);
            case Error.AVS_DECLINE /*7*/:
                return this.SP;
            case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                return this.SQ;
            case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                return this.FE;
            case CommonStatusCodes.DATE_INVALID /*12*/:
                return Integer.valueOf(this.lu);
            case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                return this.uS;
            case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                return this.SR;
            case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                return Boolean.valueOf(this.SS);
            case 18:
                return this.ST;
            case TimeUtils.HUNDRED_DAY_FIELD_LEN /*19*/:
                return this.SU;
            case 20:
                return this.SV;
            case 21:
                return Integer.valueOf(this.SW);
            case 22:
                return this.SX;
            case 23:
                return this.SY;
            case 24:
                return Integer.valueOf(this.SZ);
            case 25:
                return Integer.valueOf(this.Ta);
            case 26:
                return this.Tb;
            case 27:
                return this.pS;
            case 28:
                return this.Tc;
            case 29:
                return Boolean.valueOf(this.Td);
            default:
                throw new IllegalStateException("Unknown safe parcelable id=" + aVar.eu());
        }
    }

    public int describeContents() {
        is isVar = CREATOR;
        return 0;
    }

    public HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> en() {
        return RL;
    }

    public boolean equals(Object obj) {
        if (!(obj instanceof ir)) {
            return false;
        }
        if (this == obj) {
            return true;
        }
        ir irVar = (ir) obj;
        for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
            if (a(aVar)) {
                if (!irVar.a(aVar)) {
                    return false;
                }
                if (!b(aVar).equals(irVar.b(aVar))) {
                    return false;
                }
            } else if (irVar.a(aVar)) {
                return false;
            }
        }
        return true;
    }

    public /* synthetic */ Object freeze() {
        return id();
    }

    public String getAboutMe() {
        return this.SK;
    }

    public AgeRange getAgeRange() {
        return this.SL;
    }

    public String getBirthday() {
        return this.SM;
    }

    public String getBraggingRights() {
        return this.SN;
    }

    public int getCircledByCount() {
        return this.SO;
    }

    public Cover getCover() {
        return this.SP;
    }

    public String getCurrentLocation() {
        return this.SQ;
    }

    public String getDisplayName() {
        return this.FE;
    }

    public int getGender() {
        return this.lu;
    }

    public String getId() {
        return this.uS;
    }

    public Image getImage() {
        return this.SR;
    }

    public String getLanguage() {
        return this.ST;
    }

    public Name getName() {
        return this.SU;
    }

    public String getNickname() {
        return this.SV;
    }

    public int getObjectType() {
        return this.SW;
    }

    public List<Organizations> getOrganizations() {
        return (ArrayList) this.SX;
    }

    public List<PlacesLived> getPlacesLived() {
        return (ArrayList) this.SY;
    }

    public int getPlusOneCount() {
        return this.SZ;
    }

    public int getRelationshipStatus() {
        return this.Ta;
    }

    public String getTagline() {
        return this.Tb;
    }

    public String getUrl() {
        return this.pS;
    }

    public List<Urls> getUrls() {
        return (ArrayList) this.Tc;
    }

    int getVersionCode() {
        return this.wj;
    }

    Set<Integer> hB() {
        return this.RM;
    }

    a hW() {
        return this.SL;
    }

    b hX() {
        return this.SP;
    }

    c hY() {
        return this.SR;
    }

    d hZ() {
        return this.SU;
    }

    public boolean hasAboutMe() {
        return this.RM.contains(Integer.valueOf(2));
    }

    public boolean hasAgeRange() {
        return this.RM.contains(Integer.valueOf(3));
    }

    public boolean hasBirthday() {
        return this.RM.contains(Integer.valueOf(4));
    }

    public boolean hasBraggingRights() {
        return this.RM.contains(Integer.valueOf(5));
    }

    public boolean hasCircledByCount() {
        return this.RM.contains(Integer.valueOf(6));
    }

    public boolean hasCover() {
        return this.RM.contains(Integer.valueOf(7));
    }

    public boolean hasCurrentLocation() {
        return this.RM.contains(Integer.valueOf(8));
    }

    public boolean hasDisplayName() {
        return this.RM.contains(Integer.valueOf(9));
    }

    public boolean hasGender() {
        return this.RM.contains(Integer.valueOf(12));
    }

    public boolean hasId() {
        return this.RM.contains(Integer.valueOf(14));
    }

    public boolean hasImage() {
        return this.RM.contains(Integer.valueOf(15));
    }

    public boolean hasIsPlusUser() {
        return this.RM.contains(Integer.valueOf(16));
    }

    public boolean hasLanguage() {
        return this.RM.contains(Integer.valueOf(18));
    }

    public boolean hasName() {
        return this.RM.contains(Integer.valueOf(19));
    }

    public boolean hasNickname() {
        return this.RM.contains(Integer.valueOf(20));
    }

    public boolean hasObjectType() {
        return this.RM.contains(Integer.valueOf(21));
    }

    public boolean hasOrganizations() {
        return this.RM.contains(Integer.valueOf(22));
    }

    public boolean hasPlacesLived() {
        return this.RM.contains(Integer.valueOf(23));
    }

    public boolean hasPlusOneCount() {
        return this.RM.contains(Integer.valueOf(24));
    }

    public boolean hasRelationshipStatus() {
        return this.RM.contains(Integer.valueOf(25));
    }

    public boolean hasTagline() {
        return this.RM.contains(Integer.valueOf(26));
    }

    public boolean hasUrl() {
        return this.RM.contains(Integer.valueOf(27));
    }

    public boolean hasUrls() {
        return this.RM.contains(Integer.valueOf(28));
    }

    public boolean hasVerified() {
        return this.RM.contains(Integer.valueOf(29));
    }

    public int hashCode() {
        int i = 0;
        for (com.google.android.gms.internal.fb.a aVar : RL.values()) {
            int hashCode;
            if (a(aVar)) {
                hashCode = b(aVar).hashCode() + (i + aVar.eu());
            } else {
                hashCode = i;
            }
            i = hashCode;
        }
        return i;
    }

    List<f> ia() {
        return this.SX;
    }

    List<g> ib() {
        return this.SY;
    }

    List<h> ic() {
        return this.Tc;
    }

    public ir id() {
        return this;
    }

    public boolean isDataValid() {
        return true;
    }

    public boolean isPlusUser() {
        return this.SS;
    }

    public boolean isVerified() {
        return this.Td;
    }

    public void writeToParcel(Parcel out, int flags) {
        is isVar = CREATOR;
        is.a(this, out, flags);
    }
}
