package com.google.android.gms.internal;

import android.support.v4.media.TransportMediator;
import com.google.ads.AdSize;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.io.IOException;

public interface d {

    public static final class a extends ka<a> {
        private static volatile a[] fX;
        public String fY;
        public a[] fZ;
        public a[] ga;
        public a[] gb;
        public String gc;
        public String gd;
        public long ge;
        public boolean gf;
        public a[] gg;
        public int[] gh;
        public boolean gi;
        public int type;

        public a() {
            s();
        }

        public static a[] r() {
            if (fX == null) {
                synchronized (kc.aah) {
                    if (fX == null) {
                        fX = new a[0];
                    }
                }
            }
            return fX;
        }

        public void a(jz jzVar) throws IOException {
            int i = 0;
            jzVar.f(1, this.type);
            if (!this.fY.equals("")) {
                jzVar.b(2, this.fY);
            }
            if (this.fZ != null && this.fZ.length > 0) {
                for (ke keVar : this.fZ) {
                    if (keVar != null) {
                        jzVar.a(3, keVar);
                    }
                }
            }
            if (this.ga != null && this.ga.length > 0) {
                for (ke keVar2 : this.ga) {
                    if (keVar2 != null) {
                        jzVar.a(4, keVar2);
                    }
                }
            }
            if (this.gb != null && this.gb.length > 0) {
                for (ke keVar22 : this.gb) {
                    if (keVar22 != null) {
                        jzVar.a(5, keVar22);
                    }
                }
            }
            if (!this.gc.equals("")) {
                jzVar.b(6, this.gc);
            }
            if (!this.gd.equals("")) {
                jzVar.b(7, this.gd);
            }
            if (this.ge != 0) {
                jzVar.b(8, this.ge);
            }
            if (this.gi) {
                jzVar.a(9, this.gi);
            }
            if (this.gh != null && this.gh.length > 0) {
                for (int f : this.gh) {
                    jzVar.f(10, f);
                }
            }
            if (this.gg != null && this.gg.length > 0) {
                while (i < this.gg.length) {
                    ke keVar3 = this.gg[i];
                    if (keVar3 != null) {
                        jzVar.a(11, keVar3);
                    }
                    i++;
                }
            }
            if (this.gf) {
                jzVar.a(12, this.gf);
            }
            super.a(jzVar);
        }

        public /* synthetic */ ke b(jy jyVar) throws IOException {
            return l(jyVar);
        }

        public int c() {
            int i;
            int i2 = 0;
            int c = super.c() + jz.g(1, this.type);
            if (!this.fY.equals("")) {
                c += jz.g(2, this.fY);
            }
            if (this.fZ != null && this.fZ.length > 0) {
                i = c;
                for (ke keVar : this.fZ) {
                    if (keVar != null) {
                        i += jz.b(3, keVar);
                    }
                }
                c = i;
            }
            if (this.ga != null && this.ga.length > 0) {
                i = c;
                for (ke keVar2 : this.ga) {
                    if (keVar2 != null) {
                        i += jz.b(4, keVar2);
                    }
                }
                c = i;
            }
            if (this.gb != null && this.gb.length > 0) {
                i = c;
                for (ke keVar22 : this.gb) {
                    if (keVar22 != null) {
                        i += jz.b(5, keVar22);
                    }
                }
                c = i;
            }
            if (!this.gc.equals("")) {
                c += jz.g(6, this.gc);
            }
            if (!this.gd.equals("")) {
                c += jz.g(7, this.gd);
            }
            if (this.ge != 0) {
                c += jz.d(8, this.ge);
            }
            if (this.gi) {
                c += jz.b(9, this.gi);
            }
            if (this.gh != null && this.gh.length > 0) {
                int i3 = 0;
                for (int cC : this.gh) {
                    i3 += jz.cC(cC);
                }
                c = (c + i3) + (this.gh.length * 1);
            }
            if (this.gg != null && this.gg.length > 0) {
                while (i2 < this.gg.length) {
                    ke keVar3 = this.gg[i2];
                    if (keVar3 != null) {
                        c += jz.b(11, keVar3);
                    }
                    i2++;
                }
            }
            if (this.gf) {
                c += jz.b(12, this.gf);
            }
            this.DY = c;
            return c;
        }

        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof a)) {
                return false;
            }
            a aVar = (a) o;
            if (this.type != aVar.type) {
                return false;
            }
            if (this.fY == null) {
                if (aVar.fY != null) {
                    return false;
                }
            } else if (!this.fY.equals(aVar.fY)) {
                return false;
            }
            if (!kc.equals(this.fZ, aVar.fZ) || !kc.equals(this.ga, aVar.ga) || !kc.equals(this.gb, aVar.gb)) {
                return false;
            }
            if (this.gc == null) {
                if (aVar.gc != null) {
                    return false;
                }
            } else if (!this.gc.equals(aVar.gc)) {
                return false;
            }
            if (this.gd == null) {
                if (aVar.gd != null) {
                    return false;
                }
            } else if (!this.gd.equals(aVar.gd)) {
                return false;
            }
            if (this.ge != aVar.ge || this.gf != aVar.gf || !kc.equals(this.gg, aVar.gg) || !kc.equals(this.gh, aVar.gh) || this.gi != aVar.gi) {
                return false;
            }
            if (this.aae == null || this.aae.isEmpty()) {
                return aVar.aae == null || aVar.aae.isEmpty();
            } else {
                return this.aae.equals(aVar.aae);
            }
        }

        public int hashCode() {
            int i = 1231;
            int i2 = 0;
            int hashCode = ((((((this.gf ? 1231 : 1237) + (((((this.gd == null ? 0 : this.gd.hashCode()) + (((this.gc == null ? 0 : this.gc.hashCode()) + (((((((((this.fY == null ? 0 : this.fY.hashCode()) + ((this.type + 527) * 31)) * 31) + kc.hashCode(this.fZ)) * 31) + kc.hashCode(this.ga)) * 31) + kc.hashCode(this.gb)) * 31)) * 31)) * 31) + ((int) (this.ge ^ (this.ge >>> 32)))) * 31)) * 31) + kc.hashCode(this.gg)) * 31) + kc.hashCode(this.gh)) * 31;
            if (!this.gi) {
                i = 1237;
            }
            hashCode = (hashCode + i) * 31;
            if (!(this.aae == null || this.aae.isEmpty())) {
                i2 = this.aae.hashCode();
            }
            return hashCode + i2;
        }

        public a l(jy jyVar) throws IOException {
            while (true) {
                int ky = jyVar.ky();
                int c;
                Object obj;
                int i;
                switch (ky) {
                    case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                        break;
                    case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                        ky = jyVar.kB();
                        switch (ky) {
                            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                            case DetectedActivity.ON_FOOT /*2*/:
                            case DetectedActivity.STILL /*3*/:
                            case DetectedActivity.UNKNOWN /*4*/:
                            case DetectedActivity.TILTING /*5*/:
                            case Participant.STATUS_UNRESPONSIVE /*6*/:
                            case Error.AVS_DECLINE /*7*/:
                            case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                                this.type = ky;
                                break;
                            default:
                                continue;
                        }
                    case 18:
                        this.fY = jyVar.readString();
                        continue;
                    case 26:
                        c = kh.c(jyVar, 26);
                        ky = this.fZ == null ? 0 : this.fZ.length;
                        obj = new a[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fZ, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = new a();
                            jyVar.a(obj[ky]);
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = new a();
                        jyVar.a(obj[ky]);
                        this.fZ = obj;
                        continue;
                    case 34:
                        c = kh.c(jyVar, 34);
                        ky = this.ga == null ? 0 : this.ga.length;
                        obj = new a[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.ga, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = new a();
                            jyVar.a(obj[ky]);
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = new a();
                        jyVar.a(obj[ky]);
                        this.ga = obj;
                        continue;
                    case 42:
                        c = kh.c(jyVar, 42);
                        ky = this.gb == null ? 0 : this.gb.length;
                        obj = new a[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.gb, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = new a();
                            jyVar.a(obj[ky]);
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = new a();
                        jyVar.a(obj[ky]);
                        this.gb = obj;
                        continue;
                    case AdSize.PORTRAIT_AD_HEIGHT /*50*/:
                        this.gc = jyVar.readString();
                        continue;
                    case 58:
                        this.gd = jyVar.readString();
                        continue;
                    case TransportMediator.FLAG_KEY_MEDIA_FAST_FORWARD /*64*/:
                        this.ge = jyVar.kA();
                        continue;
                    case 72:
                        this.gi = jyVar.kC();
                        continue;
                    case 80:
                        int c2 = kh.c(jyVar, 80);
                        Object obj2 = new int[c2];
                        i = 0;
                        c = 0;
                        while (i < c2) {
                            if (i != 0) {
                                jyVar.ky();
                            }
                            int kB = jyVar.kB();
                            switch (kB) {
                                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                                case DetectedActivity.ON_FOOT /*2*/:
                                case DetectedActivity.STILL /*3*/:
                                case DetectedActivity.UNKNOWN /*4*/:
                                case DetectedActivity.TILTING /*5*/:
                                case Participant.STATUS_UNRESPONSIVE /*6*/:
                                case Error.AVS_DECLINE /*7*/:
                                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                                case CommonStatusCodes.DATE_INVALID /*12*/:
                                case CommonStatusCodes.ERROR /*13*/:
                                case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                                case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                                case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                                case 17:
                                    ky = c + 1;
                                    obj2[c] = kB;
                                    break;
                                default:
                                    ky = c;
                                    break;
                            }
                            i++;
                            c = ky;
                        }
                        if (c != 0) {
                            ky = this.gh == null ? 0 : this.gh.length;
                            if (ky != 0 || c != obj2.length) {
                                Object obj3 = new int[(ky + c)];
                                if (ky != 0) {
                                    System.arraycopy(this.gh, 0, obj3, 0, ky);
                                }
                                System.arraycopy(obj2, 0, obj3, ky, c);
                                this.gh = obj3;
                                break;
                            }
                            this.gh = obj2;
                            break;
                        }
                        continue;
                    case 82:
                        i = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            switch (jyVar.kB()) {
                                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                                case DetectedActivity.ON_FOOT /*2*/:
                                case DetectedActivity.STILL /*3*/:
                                case DetectedActivity.UNKNOWN /*4*/:
                                case DetectedActivity.TILTING /*5*/:
                                case Participant.STATUS_UNRESPONSIVE /*6*/:
                                case Error.AVS_DECLINE /*7*/:
                                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                                case CommonStatusCodes.DATE_INVALID /*12*/:
                                case CommonStatusCodes.ERROR /*13*/:
                                case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                                case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                                case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                                case 17:
                                    ky++;
                                    break;
                                default:
                                    break;
                            }
                        }
                        if (ky != 0) {
                            jyVar.cy(c);
                            c = this.gh == null ? 0 : this.gh.length;
                            Object obj4 = new int[(ky + c)];
                            if (c != 0) {
                                System.arraycopy(this.gh, 0, obj4, 0, c);
                            }
                            while (jyVar.kJ() > 0) {
                                int kB2 = jyVar.kB();
                                switch (kB2) {
                                    case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                                    case DetectedActivity.ON_FOOT /*2*/:
                                    case DetectedActivity.STILL /*3*/:
                                    case DetectedActivity.UNKNOWN /*4*/:
                                    case DetectedActivity.TILTING /*5*/:
                                    case Participant.STATUS_UNRESPONSIVE /*6*/:
                                    case Error.AVS_DECLINE /*7*/:
                                    case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                                    case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                                    case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                                    case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                                    case CommonStatusCodes.DATE_INVALID /*12*/:
                                    case CommonStatusCodes.ERROR /*13*/:
                                    case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                                    case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                                    case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                                    case 17:
                                        ky = c + 1;
                                        obj4[c] = kB2;
                                        c = ky;
                                        break;
                                    default:
                                        break;
                                }
                            }
                            this.gh = obj4;
                        }
                        jyVar.cx(i);
                        continue;
                    case AdSize.LARGE_AD_HEIGHT /*90*/:
                        c = kh.c(jyVar, 90);
                        ky = this.gg == null ? 0 : this.gg.length;
                        obj = new a[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.gg, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = new a();
                            jyVar.a(obj[ky]);
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = new a();
                        jyVar.a(obj[ky]);
                        this.gg = obj;
                        continue;
                    case 96:
                        this.gf = jyVar.kC();
                        continue;
                    default:
                        if (!a(jyVar, ky)) {
                            break;
                        }
                        continue;
                }
                return this;
            }
        }

        public a s() {
            this.type = 1;
            this.fY = "";
            this.fZ = r();
            this.ga = r();
            this.gb = r();
            this.gc = "";
            this.gd = "";
            this.ge = 0;
            this.gf = false;
            this.gg = r();
            this.gh = kh.aaj;
            this.gi = false;
            this.aae = null;
            this.DY = -1;
            return this;
        }
    }
}
