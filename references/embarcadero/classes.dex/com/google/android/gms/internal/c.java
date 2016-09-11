package com.google.android.gms.internal;

import android.support.v4.media.TransportMediator;
import com.google.ads.AdSize;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.io.IOException;

public interface c {

    public static final class a extends ka<a> {
        public int eP;
        public int eQ;
        public int level;

        public a() {
            b();
        }

        public a a(jy jyVar) throws IOException {
            while (true) {
                int ky = jyVar.ky();
                switch (ky) {
                    case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                        break;
                    case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                        ky = jyVar.kB();
                        switch (ky) {
                            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                            case DetectedActivity.ON_FOOT /*2*/:
                            case DetectedActivity.STILL /*3*/:
                                this.level = ky;
                                break;
                            default:
                                continue;
                        }
                    case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                        this.eP = jyVar.kB();
                        continue;
                    case 24:
                        this.eQ = jyVar.kB();
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

        public void a(jz jzVar) throws IOException {
            if (this.level != 1) {
                jzVar.f(1, this.level);
            }
            if (this.eP != 0) {
                jzVar.f(2, this.eP);
            }
            if (this.eQ != 0) {
                jzVar.f(3, this.eQ);
            }
            super.a(jzVar);
        }

        public a b() {
            this.level = 1;
            this.eP = 0;
            this.eQ = 0;
            this.aae = null;
            this.DY = -1;
            return this;
        }

        public /* synthetic */ ke b(jy jyVar) throws IOException {
            return a(jyVar);
        }

        public int c() {
            int c = super.c();
            if (this.level != 1) {
                c += jz.g(1, this.level);
            }
            if (this.eP != 0) {
                c += jz.g(2, this.eP);
            }
            if (this.eQ != 0) {
                c += jz.g(3, this.eQ);
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
            if (this.level != aVar.level || this.eP != aVar.eP || this.eQ != aVar.eQ) {
                return false;
            }
            if (this.aae == null || this.aae.isEmpty()) {
                return aVar.aae == null || aVar.aae.isEmpty();
            } else {
                return this.aae.equals(aVar.aae);
            }
        }

        public int hashCode() {
            int i = (((((this.level + 527) * 31) + this.eP) * 31) + this.eQ) * 31;
            int hashCode = (this.aae == null || this.aae.isEmpty()) ? 0 : this.aae.hashCode();
            return hashCode + i;
        }
    }

    public static final class b extends ka<b> {
        private static volatile b[] eR;
        public int[] eS;
        public int eT;
        public boolean eU;
        public boolean eV;
        public int name;

        public b() {
            e();
        }

        public static b[] d() {
            if (eR == null) {
                synchronized (kc.aah) {
                    if (eR == null) {
                        eR = new b[0];
                    }
                }
            }
            return eR;
        }

        public void a(jz jzVar) throws IOException {
            if (this.eV) {
                jzVar.a(1, this.eV);
            }
            jzVar.f(2, this.eT);
            if (this.eS != null && this.eS.length > 0) {
                for (int f : this.eS) {
                    jzVar.f(3, f);
                }
            }
            if (this.name != 0) {
                jzVar.f(4, this.name);
            }
            if (this.eU) {
                jzVar.a(6, this.eU);
            }
            super.a(jzVar);
        }

        public /* synthetic */ ke b(jy jyVar) throws IOException {
            return c(jyVar);
        }

        public int c() {
            int i = 0;
            int c = super.c();
            if (this.eV) {
                c += jz.b(1, this.eV);
            }
            int g = jz.g(2, this.eT) + c;
            if (this.eS == null || this.eS.length <= 0) {
                c = g;
            } else {
                for (int cC : this.eS) {
                    i += jz.cC(cC);
                }
                c = (g + i) + (this.eS.length * 1);
            }
            if (this.name != 0) {
                c += jz.g(4, this.name);
            }
            if (this.eU) {
                c += jz.b(6, this.eU);
            }
            this.DY = c;
            return c;
        }

        public b c(jy jyVar) throws IOException {
            while (true) {
                int ky = jyVar.ky();
                int c;
                switch (ky) {
                    case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                        break;
                    case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                        this.eV = jyVar.kC();
                        continue;
                    case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                        this.eT = jyVar.kB();
                        continue;
                    case 24:
                        c = kh.c(jyVar, 24);
                        ky = this.eS == null ? 0 : this.eS.length;
                        Object obj = new int[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.eS, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.kB();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.kB();
                        this.eS = obj;
                        continue;
                    case 26:
                        int cw = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            jyVar.kB();
                            ky++;
                        }
                        jyVar.cy(c);
                        c = this.eS == null ? 0 : this.eS.length;
                        Object obj2 = new int[(ky + c)];
                        if (c != 0) {
                            System.arraycopy(this.eS, 0, obj2, 0, c);
                        }
                        while (c < obj2.length) {
                            obj2[c] = jyVar.kB();
                            c++;
                        }
                        this.eS = obj2;
                        jyVar.cx(cw);
                        continue;
                    case TransportMediator.FLAG_KEY_MEDIA_STOP /*32*/:
                        this.name = jyVar.kB();
                        continue;
                    case 48:
                        this.eU = jyVar.kC();
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

        public b e() {
            this.eS = kh.aaj;
            this.eT = 0;
            this.name = 0;
            this.eU = false;
            this.eV = false;
            this.aae = null;
            this.DY = -1;
            return this;
        }

        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof b)) {
                return false;
            }
            b bVar = (b) o;
            if (!kc.equals(this.eS, bVar.eS) || this.eT != bVar.eT || this.name != bVar.name || this.eU != bVar.eU || this.eV != bVar.eV) {
                return false;
            }
            if (this.aae == null || this.aae.isEmpty()) {
                return bVar.aae == null || bVar.aae.isEmpty();
            } else {
                return this.aae.equals(bVar.aae);
            }
        }

        public int hashCode() {
            int i = 1231;
            int hashCode = ((this.eU ? 1231 : 1237) + ((((((kc.hashCode(this.eS) + 527) * 31) + this.eT) * 31) + this.name) * 31)) * 31;
            if (!this.eV) {
                i = 1237;
            }
            i = (hashCode + i) * 31;
            hashCode = (this.aae == null || this.aae.isEmpty()) ? 0 : this.aae.hashCode();
            return hashCode + i;
        }
    }

    public static final class c extends ka<c> {
        private static volatile c[] eW;
        public String eX;
        public long eY;
        public long eZ;
        public boolean fa;
        public long fb;

        public c() {
            g();
        }

        public static c[] f() {
            if (eW == null) {
                synchronized (kc.aah) {
                    if (eW == null) {
                        eW = new c[0];
                    }
                }
            }
            return eW;
        }

        public void a(jz jzVar) throws IOException {
            if (!this.eX.equals("")) {
                jzVar.b(1, this.eX);
            }
            if (this.eY != 0) {
                jzVar.b(2, this.eY);
            }
            if (this.eZ != 2147483647L) {
                jzVar.b(3, this.eZ);
            }
            if (this.fa) {
                jzVar.a(4, this.fa);
            }
            if (this.fb != 0) {
                jzVar.b(5, this.fb);
            }
            super.a(jzVar);
        }

        public /* synthetic */ ke b(jy jyVar) throws IOException {
            return d(jyVar);
        }

        public int c() {
            int c = super.c();
            if (!this.eX.equals("")) {
                c += jz.g(1, this.eX);
            }
            if (this.eY != 0) {
                c += jz.d(2, this.eY);
            }
            if (this.eZ != 2147483647L) {
                c += jz.d(3, this.eZ);
            }
            if (this.fa) {
                c += jz.b(4, this.fa);
            }
            if (this.fb != 0) {
                c += jz.d(5, this.fb);
            }
            this.DY = c;
            return c;
        }

        public c d(jy jyVar) throws IOException {
            while (true) {
                int ky = jyVar.ky();
                switch (ky) {
                    case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                        break;
                    case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                        this.eX = jyVar.readString();
                        continue;
                    case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                        this.eY = jyVar.kA();
                        continue;
                    case 24:
                        this.eZ = jyVar.kA();
                        continue;
                    case TransportMediator.FLAG_KEY_MEDIA_STOP /*32*/:
                        this.fa = jyVar.kC();
                        continue;
                    case 40:
                        this.fb = jyVar.kA();
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

        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof c)) {
                return false;
            }
            c cVar = (c) o;
            if (this.eX == null) {
                if (cVar.eX != null) {
                    return false;
                }
            } else if (!this.eX.equals(cVar.eX)) {
                return false;
            }
            if (this.eY != cVar.eY || this.eZ != cVar.eZ || this.fa != cVar.fa || this.fb != cVar.fb) {
                return false;
            }
            if (this.aae == null || this.aae.isEmpty()) {
                return cVar.aae == null || cVar.aae.isEmpty();
            } else {
                return this.aae.equals(cVar.aae);
            }
        }

        public c g() {
            this.eX = "";
            this.eY = 0;
            this.eZ = 2147483647L;
            this.fa = false;
            this.fb = 0;
            this.aae = null;
            this.DY = -1;
            return this;
        }

        public int hashCode() {
            int i = 0;
            int hashCode = ((((this.fa ? 1231 : 1237) + (((((((this.eX == null ? 0 : this.eX.hashCode()) + 527) * 31) + ((int) (this.eY ^ (this.eY >>> 32)))) * 31) + ((int) (this.eZ ^ (this.eZ >>> 32)))) * 31)) * 31) + ((int) (this.fb ^ (this.fb >>> 32)))) * 31;
            if (!(this.aae == null || this.aae.isEmpty())) {
                i = this.aae.hashCode();
            }
            return hashCode + i;
        }
    }

    public static final class d extends ka<d> {
        public com.google.android.gms.internal.d.a[] fc;
        public com.google.android.gms.internal.d.a[] fd;
        public c[] fe;

        public d() {
            h();
        }

        public void a(jz jzVar) throws IOException {
            int i = 0;
            if (this.fc != null && this.fc.length > 0) {
                for (ke keVar : this.fc) {
                    if (keVar != null) {
                        jzVar.a(1, keVar);
                    }
                }
            }
            if (this.fd != null && this.fd.length > 0) {
                for (ke keVar2 : this.fd) {
                    if (keVar2 != null) {
                        jzVar.a(2, keVar2);
                    }
                }
            }
            if (this.fe != null && this.fe.length > 0) {
                while (i < this.fe.length) {
                    ke keVar3 = this.fe[i];
                    if (keVar3 != null) {
                        jzVar.a(3, keVar3);
                    }
                    i++;
                }
            }
            super.a(jzVar);
        }

        public /* synthetic */ ke b(jy jyVar) throws IOException {
            return e(jyVar);
        }

        public int c() {
            int i;
            int i2 = 0;
            int c = super.c();
            if (this.fc != null && this.fc.length > 0) {
                i = c;
                for (ke keVar : this.fc) {
                    if (keVar != null) {
                        i += jz.b(1, keVar);
                    }
                }
                c = i;
            }
            if (this.fd != null && this.fd.length > 0) {
                i = c;
                for (ke keVar2 : this.fd) {
                    if (keVar2 != null) {
                        i += jz.b(2, keVar2);
                    }
                }
                c = i;
            }
            if (this.fe != null && this.fe.length > 0) {
                while (i2 < this.fe.length) {
                    ke keVar3 = this.fe[i2];
                    if (keVar3 != null) {
                        c += jz.b(3, keVar3);
                    }
                    i2++;
                }
            }
            this.DY = c;
            return c;
        }

        public d e(jy jyVar) throws IOException {
            while (true) {
                int ky = jyVar.ky();
                int c;
                Object obj;
                switch (ky) {
                    case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                        break;
                    case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                        c = kh.c(jyVar, 10);
                        ky = this.fc == null ? 0 : this.fc.length;
                        obj = new com.google.android.gms.internal.d.a[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fc, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = new com.google.android.gms.internal.d.a();
                            jyVar.a(obj[ky]);
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = new com.google.android.gms.internal.d.a();
                        jyVar.a(obj[ky]);
                        this.fc = obj;
                        continue;
                    case 18:
                        c = kh.c(jyVar, 18);
                        ky = this.fd == null ? 0 : this.fd.length;
                        obj = new com.google.android.gms.internal.d.a[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fd, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = new com.google.android.gms.internal.d.a();
                            jyVar.a(obj[ky]);
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = new com.google.android.gms.internal.d.a();
                        jyVar.a(obj[ky]);
                        this.fd = obj;
                        continue;
                    case 26:
                        c = kh.c(jyVar, 26);
                        ky = this.fe == null ? 0 : this.fe.length;
                        obj = new c[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fe, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = new c();
                            jyVar.a(obj[ky]);
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = new c();
                        jyVar.a(obj[ky]);
                        this.fe = obj;
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

        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof d)) {
                return false;
            }
            d dVar = (d) o;
            if (!kc.equals(this.fc, dVar.fc) || !kc.equals(this.fd, dVar.fd) || !kc.equals(this.fe, dVar.fe)) {
                return false;
            }
            if (this.aae == null || this.aae.isEmpty()) {
                return dVar.aae == null || dVar.aae.isEmpty();
            } else {
                return this.aae.equals(dVar.aae);
            }
        }

        public d h() {
            this.fc = com.google.android.gms.internal.d.a.r();
            this.fd = com.google.android.gms.internal.d.a.r();
            this.fe = c.f();
            this.aae = null;
            this.DY = -1;
            return this;
        }

        public int hashCode() {
            int hashCode = (((((kc.hashCode(this.fc) + 527) * 31) + kc.hashCode(this.fd)) * 31) + kc.hashCode(this.fe)) * 31;
            int hashCode2 = (this.aae == null || this.aae.isEmpty()) ? 0 : this.aae.hashCode();
            return hashCode2 + hashCode;
        }
    }

    public static final class e extends ka<e> {
        private static volatile e[] ff;
        public int key;
        public int value;

        public e() {
            j();
        }

        public static e[] i() {
            if (ff == null) {
                synchronized (kc.aah) {
                    if (ff == null) {
                        ff = new e[0];
                    }
                }
            }
            return ff;
        }

        public void a(jz jzVar) throws IOException {
            jzVar.f(1, this.key);
            jzVar.f(2, this.value);
            super.a(jzVar);
        }

        public /* synthetic */ ke b(jy jyVar) throws IOException {
            return f(jyVar);
        }

        public int c() {
            int c = (super.c() + jz.g(1, this.key)) + jz.g(2, this.value);
            this.DY = c;
            return c;
        }

        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof e)) {
                return false;
            }
            e eVar = (e) o;
            if (this.key != eVar.key || this.value != eVar.value) {
                return false;
            }
            if (this.aae == null || this.aae.isEmpty()) {
                return eVar.aae == null || eVar.aae.isEmpty();
            } else {
                return this.aae.equals(eVar.aae);
            }
        }

        public e f(jy jyVar) throws IOException {
            while (true) {
                int ky = jyVar.ky();
                switch (ky) {
                    case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                        break;
                    case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                        this.key = jyVar.kB();
                        continue;
                    case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                        this.value = jyVar.kB();
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

        public int hashCode() {
            int i = (((this.key + 527) * 31) + this.value) * 31;
            int hashCode = (this.aae == null || this.aae.isEmpty()) ? 0 : this.aae.hashCode();
            return hashCode + i;
        }

        public e j() {
            this.key = 0;
            this.value = 0;
            this.aae = null;
            this.DY = -1;
            return this;
        }
    }

    public static final class f extends ka<f> {
        public String[] fg;
        public String[] fh;
        public com.google.android.gms.internal.d.a[] fi;
        public e[] fj;
        public b[] fk;
        public b[] fl;
        public b[] fm;
        public g[] fn;
        public String fo;
        public String fp;
        public String fq;
        public String fr;
        public a fs;
        public float ft;
        public boolean fu;
        public String[] fv;
        public int fw;

        public f() {
            k();
        }

        public static f a(byte[] bArr) throws kd {
            return (f) ke.a(new f(), bArr);
        }

        public void a(jz jzVar) throws IOException {
            int i = 0;
            if (this.fh != null && this.fh.length > 0) {
                for (String str : this.fh) {
                    if (str != null) {
                        jzVar.b(1, str);
                    }
                }
            }
            if (this.fi != null && this.fi.length > 0) {
                for (ke keVar : this.fi) {
                    if (keVar != null) {
                        jzVar.a(2, keVar);
                    }
                }
            }
            if (this.fj != null && this.fj.length > 0) {
                for (ke keVar2 : this.fj) {
                    if (keVar2 != null) {
                        jzVar.a(3, keVar2);
                    }
                }
            }
            if (this.fk != null && this.fk.length > 0) {
                for (ke keVar22 : this.fk) {
                    if (keVar22 != null) {
                        jzVar.a(4, keVar22);
                    }
                }
            }
            if (this.fl != null && this.fl.length > 0) {
                for (ke keVar222 : this.fl) {
                    if (keVar222 != null) {
                        jzVar.a(5, keVar222);
                    }
                }
            }
            if (this.fm != null && this.fm.length > 0) {
                for (ke keVar2222 : this.fm) {
                    if (keVar2222 != null) {
                        jzVar.a(6, keVar2222);
                    }
                }
            }
            if (this.fn != null && this.fn.length > 0) {
                for (ke keVar22222 : this.fn) {
                    if (keVar22222 != null) {
                        jzVar.a(7, keVar22222);
                    }
                }
            }
            if (!this.fo.equals("")) {
                jzVar.b(9, this.fo);
            }
            if (!this.fp.equals("")) {
                jzVar.b(10, this.fp);
            }
            if (!this.fq.equals("0")) {
                jzVar.b(12, this.fq);
            }
            if (!this.fr.equals("")) {
                jzVar.b(13, this.fr);
            }
            if (this.fs != null) {
                jzVar.a(14, this.fs);
            }
            if (Float.floatToIntBits(this.ft) != Float.floatToIntBits(0.0f)) {
                jzVar.a(15, this.ft);
            }
            if (this.fv != null && this.fv.length > 0) {
                for (String str2 : this.fv) {
                    if (str2 != null) {
                        jzVar.b(16, str2);
                    }
                }
            }
            if (this.fw != 0) {
                jzVar.f(17, this.fw);
            }
            if (this.fu) {
                jzVar.a(18, this.fu);
            }
            if (this.fg != null && this.fg.length > 0) {
                while (i < this.fg.length) {
                    String str3 = this.fg[i];
                    if (str3 != null) {
                        jzVar.b(19, str3);
                    }
                    i++;
                }
            }
            super.a(jzVar);
        }

        public /* synthetic */ ke b(jy jyVar) throws IOException {
            return g(jyVar);
        }

        public int c() {
            int i;
            int i2;
            int i3;
            int i4 = 0;
            int c = super.c();
            if (this.fh == null || this.fh.length <= 0) {
                i = c;
            } else {
                i2 = 0;
                i3 = 0;
                for (String str : this.fh) {
                    if (str != null) {
                        i3++;
                        i2 += jz.bQ(str);
                    }
                }
                i = (c + i2) + (i3 * 1);
            }
            if (this.fi != null && this.fi.length > 0) {
                i2 = i;
                for (ke keVar : this.fi) {
                    if (keVar != null) {
                        i2 += jz.b(2, keVar);
                    }
                }
                i = i2;
            }
            if (this.fj != null && this.fj.length > 0) {
                i2 = i;
                for (ke keVar2 : this.fj) {
                    if (keVar2 != null) {
                        i2 += jz.b(3, keVar2);
                    }
                }
                i = i2;
            }
            if (this.fk != null && this.fk.length > 0) {
                i2 = i;
                for (ke keVar22 : this.fk) {
                    if (keVar22 != null) {
                        i2 += jz.b(4, keVar22);
                    }
                }
                i = i2;
            }
            if (this.fl != null && this.fl.length > 0) {
                i2 = i;
                for (ke keVar222 : this.fl) {
                    if (keVar222 != null) {
                        i2 += jz.b(5, keVar222);
                    }
                }
                i = i2;
            }
            if (this.fm != null && this.fm.length > 0) {
                i2 = i;
                for (ke keVar2222 : this.fm) {
                    if (keVar2222 != null) {
                        i2 += jz.b(6, keVar2222);
                    }
                }
                i = i2;
            }
            if (this.fn != null && this.fn.length > 0) {
                i2 = i;
                for (ke keVar22222 : this.fn) {
                    if (keVar22222 != null) {
                        i2 += jz.b(7, keVar22222);
                    }
                }
                i = i2;
            }
            if (!this.fo.equals("")) {
                i += jz.g(9, this.fo);
            }
            if (!this.fp.equals("")) {
                i += jz.g(10, this.fp);
            }
            if (!this.fq.equals("0")) {
                i += jz.g(12, this.fq);
            }
            if (!this.fr.equals("")) {
                i += jz.g(13, this.fr);
            }
            if (this.fs != null) {
                i += jz.b(14, this.fs);
            }
            if (Float.floatToIntBits(this.ft) != Float.floatToIntBits(0.0f)) {
                i += jz.b(15, this.ft);
            }
            if (this.fv != null && this.fv.length > 0) {
                i3 = 0;
                c = 0;
                for (String str2 : this.fv) {
                    if (str2 != null) {
                        c++;
                        i3 += jz.bQ(str2);
                    }
                }
                i = (i + i3) + (c * 2);
            }
            if (this.fw != 0) {
                i += jz.g(17, this.fw);
            }
            if (this.fu) {
                i += jz.b(18, this.fu);
            }
            if (this.fg != null && this.fg.length > 0) {
                i2 = 0;
                i3 = 0;
                while (i4 < this.fg.length) {
                    String str3 = this.fg[i4];
                    if (str3 != null) {
                        i3++;
                        i2 += jz.bQ(str3);
                    }
                    i4++;
                }
                i = (i + i2) + (i3 * 2);
            }
            this.DY = i;
            return i;
        }

        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof f)) {
                return false;
            }
            f fVar = (f) o;
            if (!kc.equals(this.fg, fVar.fg) || !kc.equals(this.fh, fVar.fh) || !kc.equals(this.fi, fVar.fi) || !kc.equals(this.fj, fVar.fj) || !kc.equals(this.fk, fVar.fk) || !kc.equals(this.fl, fVar.fl) || !kc.equals(this.fm, fVar.fm) || !kc.equals(this.fn, fVar.fn)) {
                return false;
            }
            if (this.fo == null) {
                if (fVar.fo != null) {
                    return false;
                }
            } else if (!this.fo.equals(fVar.fo)) {
                return false;
            }
            if (this.fp == null) {
                if (fVar.fp != null) {
                    return false;
                }
            } else if (!this.fp.equals(fVar.fp)) {
                return false;
            }
            if (this.fq == null) {
                if (fVar.fq != null) {
                    return false;
                }
            } else if (!this.fq.equals(fVar.fq)) {
                return false;
            }
            if (this.fr == null) {
                if (fVar.fr != null) {
                    return false;
                }
            } else if (!this.fr.equals(fVar.fr)) {
                return false;
            }
            if (this.fs == null) {
                if (fVar.fs != null) {
                    return false;
                }
            } else if (!this.fs.equals(fVar.fs)) {
                return false;
            }
            if (Float.floatToIntBits(this.ft) != Float.floatToIntBits(fVar.ft) || this.fu != fVar.fu || !kc.equals(this.fv, fVar.fv) || this.fw != fVar.fw) {
                return false;
            }
            if (this.aae == null || this.aae.isEmpty()) {
                return fVar.aae == null || fVar.aae.isEmpty();
            } else {
                return this.aae.equals(fVar.aae);
            }
        }

        public f g(jy jyVar) throws IOException {
            while (true) {
                int ky = jyVar.ky();
                int c;
                Object obj;
                switch (ky) {
                    case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                        break;
                    case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                        c = kh.c(jyVar, 10);
                        ky = this.fh == null ? 0 : this.fh.length;
                        obj = new String[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fh, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.readString();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.readString();
                        this.fh = obj;
                        continue;
                    case 18:
                        c = kh.c(jyVar, 18);
                        ky = this.fi == null ? 0 : this.fi.length;
                        obj = new com.google.android.gms.internal.d.a[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fi, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = new com.google.android.gms.internal.d.a();
                            jyVar.a(obj[ky]);
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = new com.google.android.gms.internal.d.a();
                        jyVar.a(obj[ky]);
                        this.fi = obj;
                        continue;
                    case 26:
                        c = kh.c(jyVar, 26);
                        ky = this.fj == null ? 0 : this.fj.length;
                        obj = new e[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fj, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = new e();
                            jyVar.a(obj[ky]);
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = new e();
                        jyVar.a(obj[ky]);
                        this.fj = obj;
                        continue;
                    case 34:
                        c = kh.c(jyVar, 34);
                        ky = this.fk == null ? 0 : this.fk.length;
                        obj = new b[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fk, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = new b();
                            jyVar.a(obj[ky]);
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = new b();
                        jyVar.a(obj[ky]);
                        this.fk = obj;
                        continue;
                    case 42:
                        c = kh.c(jyVar, 42);
                        ky = this.fl == null ? 0 : this.fl.length;
                        obj = new b[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fl, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = new b();
                            jyVar.a(obj[ky]);
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = new b();
                        jyVar.a(obj[ky]);
                        this.fl = obj;
                        continue;
                    case AdSize.PORTRAIT_AD_HEIGHT /*50*/:
                        c = kh.c(jyVar, 50);
                        ky = this.fm == null ? 0 : this.fm.length;
                        obj = new b[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fm, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = new b();
                            jyVar.a(obj[ky]);
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = new b();
                        jyVar.a(obj[ky]);
                        this.fm = obj;
                        continue;
                    case 58:
                        c = kh.c(jyVar, 58);
                        ky = this.fn == null ? 0 : this.fn.length;
                        obj = new g[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fn, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = new g();
                            jyVar.a(obj[ky]);
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = new g();
                        jyVar.a(obj[ky]);
                        this.fn = obj;
                        continue;
                    case 74:
                        this.fo = jyVar.readString();
                        continue;
                    case 82:
                        this.fp = jyVar.readString();
                        continue;
                    case 98:
                        this.fq = jyVar.readString();
                        continue;
                    case 106:
                        this.fr = jyVar.readString();
                        continue;
                    case 114:
                        if (this.fs == null) {
                            this.fs = new a();
                        }
                        jyVar.a(this.fs);
                        continue;
                    case 125:
                        this.ft = jyVar.readFloat();
                        continue;
                    case TransportMediator.KEYCODE_MEDIA_RECORD /*130*/:
                        c = kh.c(jyVar, TransportMediator.KEYCODE_MEDIA_RECORD);
                        ky = this.fv == null ? 0 : this.fv.length;
                        obj = new String[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fv, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.readString();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.readString();
                        this.fv = obj;
                        continue;
                    case 136:
                        this.fw = jyVar.kB();
                        continue;
                    case 144:
                        this.fu = jyVar.kC();
                        continue;
                    case 154:
                        c = kh.c(jyVar, 154);
                        ky = this.fg == null ? 0 : this.fg.length;
                        obj = new String[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fg, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.readString();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.readString();
                        this.fg = obj;
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

        public int hashCode() {
            int i = 0;
            int hashCode = ((((((this.fu ? 1231 : 1237) + (((((this.fs == null ? 0 : this.fs.hashCode()) + (((this.fr == null ? 0 : this.fr.hashCode()) + (((this.fq == null ? 0 : this.fq.hashCode()) + (((this.fp == null ? 0 : this.fp.hashCode()) + (((this.fo == null ? 0 : this.fo.hashCode()) + ((((((((((((((((kc.hashCode(this.fg) + 527) * 31) + kc.hashCode(this.fh)) * 31) + kc.hashCode(this.fi)) * 31) + kc.hashCode(this.fj)) * 31) + kc.hashCode(this.fk)) * 31) + kc.hashCode(this.fl)) * 31) + kc.hashCode(this.fm)) * 31) + kc.hashCode(this.fn)) * 31)) * 31)) * 31)) * 31)) * 31)) * 31) + Float.floatToIntBits(this.ft)) * 31)) * 31) + kc.hashCode(this.fv)) * 31) + this.fw) * 31;
            if (!(this.aae == null || this.aae.isEmpty())) {
                i = this.aae.hashCode();
            }
            return hashCode + i;
        }

        public f k() {
            this.fg = kh.aao;
            this.fh = kh.aao;
            this.fi = com.google.android.gms.internal.d.a.r();
            this.fj = e.i();
            this.fk = b.d();
            this.fl = b.d();
            this.fm = b.d();
            this.fn = g.l();
            this.fo = "";
            this.fp = "";
            this.fq = "0";
            this.fr = "";
            this.fs = null;
            this.ft = 0.0f;
            this.fu = false;
            this.fv = kh.aao;
            this.fw = 0;
            this.aae = null;
            this.DY = -1;
            return this;
        }
    }

    public static final class g extends ka<g> {
        private static volatile g[] fx;
        public int[] fA;
        public int[] fB;
        public int[] fC;
        public int[] fD;
        public int[] fE;
        public int[] fF;
        public int[] fG;
        public int[] fH;
        public int[] fy;
        public int[] fz;

        public g() {
            m();
        }

        public static g[] l() {
            if (fx == null) {
                synchronized (kc.aah) {
                    if (fx == null) {
                        fx = new g[0];
                    }
                }
            }
            return fx;
        }

        public void a(jz jzVar) throws IOException {
            int i = 0;
            if (this.fy != null && this.fy.length > 0) {
                for (int f : this.fy) {
                    jzVar.f(1, f);
                }
            }
            if (this.fz != null && this.fz.length > 0) {
                for (int f2 : this.fz) {
                    jzVar.f(2, f2);
                }
            }
            if (this.fA != null && this.fA.length > 0) {
                for (int f22 : this.fA) {
                    jzVar.f(3, f22);
                }
            }
            if (this.fB != null && this.fB.length > 0) {
                for (int f222 : this.fB) {
                    jzVar.f(4, f222);
                }
            }
            if (this.fC != null && this.fC.length > 0) {
                for (int f2222 : this.fC) {
                    jzVar.f(5, f2222);
                }
            }
            if (this.fD != null && this.fD.length > 0) {
                for (int f22222 : this.fD) {
                    jzVar.f(6, f22222);
                }
            }
            if (this.fE != null && this.fE.length > 0) {
                for (int f222222 : this.fE) {
                    jzVar.f(7, f222222);
                }
            }
            if (this.fF != null && this.fF.length > 0) {
                for (int f2222222 : this.fF) {
                    jzVar.f(8, f2222222);
                }
            }
            if (this.fG != null && this.fG.length > 0) {
                for (int f22222222 : this.fG) {
                    jzVar.f(9, f22222222);
                }
            }
            if (this.fH != null && this.fH.length > 0) {
                while (i < this.fH.length) {
                    jzVar.f(10, this.fH[i]);
                    i++;
                }
            }
            super.a(jzVar);
        }

        public /* synthetic */ ke b(jy jyVar) throws IOException {
            return h(jyVar);
        }

        public int c() {
            int i;
            int i2;
            int i3 = 0;
            int c = super.c();
            if (this.fy == null || this.fy.length <= 0) {
                i = c;
            } else {
                i2 = 0;
                for (int cC : this.fy) {
                    i2 += jz.cC(cC);
                }
                i = (c + i2) + (this.fy.length * 1);
            }
            if (this.fz != null && this.fz.length > 0) {
                c = 0;
                for (int cC2 : this.fz) {
                    c += jz.cC(cC2);
                }
                i = (i + c) + (this.fz.length * 1);
            }
            if (this.fA != null && this.fA.length > 0) {
                c = 0;
                for (int cC22 : this.fA) {
                    c += jz.cC(cC22);
                }
                i = (i + c) + (this.fA.length * 1);
            }
            if (this.fB != null && this.fB.length > 0) {
                c = 0;
                for (int cC222 : this.fB) {
                    c += jz.cC(cC222);
                }
                i = (i + c) + (this.fB.length * 1);
            }
            if (this.fC != null && this.fC.length > 0) {
                c = 0;
                for (int cC2222 : this.fC) {
                    c += jz.cC(cC2222);
                }
                i = (i + c) + (this.fC.length * 1);
            }
            if (this.fD != null && this.fD.length > 0) {
                c = 0;
                for (int cC22222 : this.fD) {
                    c += jz.cC(cC22222);
                }
                i = (i + c) + (this.fD.length * 1);
            }
            if (this.fE != null && this.fE.length > 0) {
                c = 0;
                for (int cC222222 : this.fE) {
                    c += jz.cC(cC222222);
                }
                i = (i + c) + (this.fE.length * 1);
            }
            if (this.fF != null && this.fF.length > 0) {
                c = 0;
                for (int cC2222222 : this.fF) {
                    c += jz.cC(cC2222222);
                }
                i = (i + c) + (this.fF.length * 1);
            }
            if (this.fG != null && this.fG.length > 0) {
                c = 0;
                for (int cC22222222 : this.fG) {
                    c += jz.cC(cC22222222);
                }
                i = (i + c) + (this.fG.length * 1);
            }
            if (this.fH != null && this.fH.length > 0) {
                i2 = 0;
                while (i3 < this.fH.length) {
                    i2 += jz.cC(this.fH[i3]);
                    i3++;
                }
                i = (i + i2) + (this.fH.length * 1);
            }
            this.DY = i;
            return i;
        }

        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof g)) {
                return false;
            }
            g gVar = (g) o;
            if (!kc.equals(this.fy, gVar.fy) || !kc.equals(this.fz, gVar.fz) || !kc.equals(this.fA, gVar.fA) || !kc.equals(this.fB, gVar.fB) || !kc.equals(this.fC, gVar.fC) || !kc.equals(this.fD, gVar.fD) || !kc.equals(this.fE, gVar.fE) || !kc.equals(this.fF, gVar.fF) || !kc.equals(this.fG, gVar.fG) || !kc.equals(this.fH, gVar.fH)) {
                return false;
            }
            if (this.aae == null || this.aae.isEmpty()) {
                return gVar.aae == null || gVar.aae.isEmpty();
            } else {
                return this.aae.equals(gVar.aae);
            }
        }

        public g h(jy jyVar) throws IOException {
            while (true) {
                int ky = jyVar.ky();
                int c;
                Object obj;
                int cw;
                Object obj2;
                switch (ky) {
                    case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                        break;
                    case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                        c = kh.c(jyVar, 8);
                        ky = this.fy == null ? 0 : this.fy.length;
                        obj = new int[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fy, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.kB();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.kB();
                        this.fy = obj;
                        continue;
                    case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                        cw = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            jyVar.kB();
                            ky++;
                        }
                        jyVar.cy(c);
                        c = this.fy == null ? 0 : this.fy.length;
                        obj2 = new int[(ky + c)];
                        if (c != 0) {
                            System.arraycopy(this.fy, 0, obj2, 0, c);
                        }
                        while (c < obj2.length) {
                            obj2[c] = jyVar.kB();
                            c++;
                        }
                        this.fy = obj2;
                        jyVar.cx(cw);
                        continue;
                    case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                        c = kh.c(jyVar, 16);
                        ky = this.fz == null ? 0 : this.fz.length;
                        obj = new int[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fz, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.kB();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.kB();
                        this.fz = obj;
                        continue;
                    case 18:
                        cw = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            jyVar.kB();
                            ky++;
                        }
                        jyVar.cy(c);
                        c = this.fz == null ? 0 : this.fz.length;
                        obj2 = new int[(ky + c)];
                        if (c != 0) {
                            System.arraycopy(this.fz, 0, obj2, 0, c);
                        }
                        while (c < obj2.length) {
                            obj2[c] = jyVar.kB();
                            c++;
                        }
                        this.fz = obj2;
                        jyVar.cx(cw);
                        continue;
                    case 24:
                        c = kh.c(jyVar, 24);
                        ky = this.fA == null ? 0 : this.fA.length;
                        obj = new int[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fA, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.kB();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.kB();
                        this.fA = obj;
                        continue;
                    case 26:
                        cw = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            jyVar.kB();
                            ky++;
                        }
                        jyVar.cy(c);
                        c = this.fA == null ? 0 : this.fA.length;
                        obj2 = new int[(ky + c)];
                        if (c != 0) {
                            System.arraycopy(this.fA, 0, obj2, 0, c);
                        }
                        while (c < obj2.length) {
                            obj2[c] = jyVar.kB();
                            c++;
                        }
                        this.fA = obj2;
                        jyVar.cx(cw);
                        continue;
                    case TransportMediator.FLAG_KEY_MEDIA_STOP /*32*/:
                        c = kh.c(jyVar, 32);
                        ky = this.fB == null ? 0 : this.fB.length;
                        obj = new int[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fB, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.kB();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.kB();
                        this.fB = obj;
                        continue;
                    case 34:
                        cw = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            jyVar.kB();
                            ky++;
                        }
                        jyVar.cy(c);
                        c = this.fB == null ? 0 : this.fB.length;
                        obj2 = new int[(ky + c)];
                        if (c != 0) {
                            System.arraycopy(this.fB, 0, obj2, 0, c);
                        }
                        while (c < obj2.length) {
                            obj2[c] = jyVar.kB();
                            c++;
                        }
                        this.fB = obj2;
                        jyVar.cx(cw);
                        continue;
                    case 40:
                        c = kh.c(jyVar, 40);
                        ky = this.fC == null ? 0 : this.fC.length;
                        obj = new int[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fC, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.kB();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.kB();
                        this.fC = obj;
                        continue;
                    case 42:
                        cw = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            jyVar.kB();
                            ky++;
                        }
                        jyVar.cy(c);
                        c = this.fC == null ? 0 : this.fC.length;
                        obj2 = new int[(ky + c)];
                        if (c != 0) {
                            System.arraycopy(this.fC, 0, obj2, 0, c);
                        }
                        while (c < obj2.length) {
                            obj2[c] = jyVar.kB();
                            c++;
                        }
                        this.fC = obj2;
                        jyVar.cx(cw);
                        continue;
                    case 48:
                        c = kh.c(jyVar, 48);
                        ky = this.fD == null ? 0 : this.fD.length;
                        obj = new int[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fD, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.kB();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.kB();
                        this.fD = obj;
                        continue;
                    case AdSize.PORTRAIT_AD_HEIGHT /*50*/:
                        cw = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            jyVar.kB();
                            ky++;
                        }
                        jyVar.cy(c);
                        c = this.fD == null ? 0 : this.fD.length;
                        obj2 = new int[(ky + c)];
                        if (c != 0) {
                            System.arraycopy(this.fD, 0, obj2, 0, c);
                        }
                        while (c < obj2.length) {
                            obj2[c] = jyVar.kB();
                            c++;
                        }
                        this.fD = obj2;
                        jyVar.cx(cw);
                        continue;
                    case 56:
                        c = kh.c(jyVar, 56);
                        ky = this.fE == null ? 0 : this.fE.length;
                        obj = new int[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fE, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.kB();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.kB();
                        this.fE = obj;
                        continue;
                    case 58:
                        cw = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            jyVar.kB();
                            ky++;
                        }
                        jyVar.cy(c);
                        c = this.fE == null ? 0 : this.fE.length;
                        obj2 = new int[(ky + c)];
                        if (c != 0) {
                            System.arraycopy(this.fE, 0, obj2, 0, c);
                        }
                        while (c < obj2.length) {
                            obj2[c] = jyVar.kB();
                            c++;
                        }
                        this.fE = obj2;
                        jyVar.cx(cw);
                        continue;
                    case TransportMediator.FLAG_KEY_MEDIA_FAST_FORWARD /*64*/:
                        c = kh.c(jyVar, 64);
                        ky = this.fF == null ? 0 : this.fF.length;
                        obj = new int[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fF, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.kB();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.kB();
                        this.fF = obj;
                        continue;
                    case 66:
                        cw = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            jyVar.kB();
                            ky++;
                        }
                        jyVar.cy(c);
                        c = this.fF == null ? 0 : this.fF.length;
                        obj2 = new int[(ky + c)];
                        if (c != 0) {
                            System.arraycopy(this.fF, 0, obj2, 0, c);
                        }
                        while (c < obj2.length) {
                            obj2[c] = jyVar.kB();
                            c++;
                        }
                        this.fF = obj2;
                        jyVar.cx(cw);
                        continue;
                    case 72:
                        c = kh.c(jyVar, 72);
                        ky = this.fG == null ? 0 : this.fG.length;
                        obj = new int[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fG, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.kB();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.kB();
                        this.fG = obj;
                        continue;
                    case 74:
                        cw = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            jyVar.kB();
                            ky++;
                        }
                        jyVar.cy(c);
                        c = this.fG == null ? 0 : this.fG.length;
                        obj2 = new int[(ky + c)];
                        if (c != 0) {
                            System.arraycopy(this.fG, 0, obj2, 0, c);
                        }
                        while (c < obj2.length) {
                            obj2[c] = jyVar.kB();
                            c++;
                        }
                        this.fG = obj2;
                        jyVar.cx(cw);
                        continue;
                    case 80:
                        c = kh.c(jyVar, 80);
                        ky = this.fH == null ? 0 : this.fH.length;
                        obj = new int[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fH, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.kB();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.kB();
                        this.fH = obj;
                        continue;
                    case 82:
                        cw = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            jyVar.kB();
                            ky++;
                        }
                        jyVar.cy(c);
                        c = this.fH == null ? 0 : this.fH.length;
                        obj2 = new int[(ky + c)];
                        if (c != 0) {
                            System.arraycopy(this.fH, 0, obj2, 0, c);
                        }
                        while (c < obj2.length) {
                            obj2[c] = jyVar.kB();
                            c++;
                        }
                        this.fH = obj2;
                        jyVar.cx(cw);
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

        public int hashCode() {
            int hashCode = (((((((((((((((((((kc.hashCode(this.fy) + 527) * 31) + kc.hashCode(this.fz)) * 31) + kc.hashCode(this.fA)) * 31) + kc.hashCode(this.fB)) * 31) + kc.hashCode(this.fC)) * 31) + kc.hashCode(this.fD)) * 31) + kc.hashCode(this.fE)) * 31) + kc.hashCode(this.fF)) * 31) + kc.hashCode(this.fG)) * 31) + kc.hashCode(this.fH)) * 31;
            int hashCode2 = (this.aae == null || this.aae.isEmpty()) ? 0 : this.aae.hashCode();
            return hashCode2 + hashCode;
        }

        public g m() {
            this.fy = kh.aaj;
            this.fz = kh.aaj;
            this.fA = kh.aaj;
            this.fB = kh.aaj;
            this.fC = kh.aaj;
            this.fD = kh.aaj;
            this.fE = kh.aaj;
            this.fF = kh.aaj;
            this.fG = kh.aaj;
            this.fH = kh.aaj;
            this.aae = null;
            this.DY = -1;
            return this;
        }
    }

    public static final class h extends ka<h> {
        public static final kb<com.google.android.gms.internal.d.a, h> fI;
        private static final h[] fJ;
        public int[] fK;
        public int[] fL;
        public int[] fM;
        public int fN;
        public int[] fO;
        public int fP;
        public int fQ;

        static {
            fI = kb.a(11, h.class, 810);
            fJ = new h[0];
        }

        public h() {
            n();
        }

        public void a(jz jzVar) throws IOException {
            int i = 0;
            if (this.fK != null && this.fK.length > 0) {
                for (int f : this.fK) {
                    jzVar.f(1, f);
                }
            }
            if (this.fL != null && this.fL.length > 0) {
                for (int f2 : this.fL) {
                    jzVar.f(2, f2);
                }
            }
            if (this.fM != null && this.fM.length > 0) {
                for (int f22 : this.fM) {
                    jzVar.f(3, f22);
                }
            }
            if (this.fN != 0) {
                jzVar.f(4, this.fN);
            }
            if (this.fO != null && this.fO.length > 0) {
                while (i < this.fO.length) {
                    jzVar.f(5, this.fO[i]);
                    i++;
                }
            }
            if (this.fP != 0) {
                jzVar.f(6, this.fP);
            }
            if (this.fQ != 0) {
                jzVar.f(7, this.fQ);
            }
            super.a(jzVar);
        }

        public /* synthetic */ ke b(jy jyVar) throws IOException {
            return i(jyVar);
        }

        public int c() {
            int i;
            int i2;
            int i3 = 0;
            int c = super.c();
            if (this.fK == null || this.fK.length <= 0) {
                i = c;
            } else {
                i2 = 0;
                for (int cC : this.fK) {
                    i2 += jz.cC(cC);
                }
                i = (c + i2) + (this.fK.length * 1);
            }
            if (this.fL != null && this.fL.length > 0) {
                c = 0;
                for (int cC2 : this.fL) {
                    c += jz.cC(cC2);
                }
                i = (i + c) + (this.fL.length * 1);
            }
            if (this.fM != null && this.fM.length > 0) {
                c = 0;
                for (int cC22 : this.fM) {
                    c += jz.cC(cC22);
                }
                i = (i + c) + (this.fM.length * 1);
            }
            if (this.fN != 0) {
                i += jz.g(4, this.fN);
            }
            if (this.fO != null && this.fO.length > 0) {
                i2 = 0;
                while (i3 < this.fO.length) {
                    i2 += jz.cC(this.fO[i3]);
                    i3++;
                }
                i = (i + i2) + (this.fO.length * 1);
            }
            if (this.fP != 0) {
                i += jz.g(6, this.fP);
            }
            if (this.fQ != 0) {
                i += jz.g(7, this.fQ);
            }
            this.DY = i;
            return i;
        }

        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof h)) {
                return false;
            }
            h hVar = (h) o;
            if (!kc.equals(this.fK, hVar.fK) || !kc.equals(this.fL, hVar.fL) || !kc.equals(this.fM, hVar.fM) || this.fN != hVar.fN || !kc.equals(this.fO, hVar.fO) || this.fP != hVar.fP || this.fQ != hVar.fQ) {
                return false;
            }
            if (this.aae == null || this.aae.isEmpty()) {
                return hVar.aae == null || hVar.aae.isEmpty();
            } else {
                return this.aae.equals(hVar.aae);
            }
        }

        public int hashCode() {
            int hashCode = (((((((((((((kc.hashCode(this.fK) + 527) * 31) + kc.hashCode(this.fL)) * 31) + kc.hashCode(this.fM)) * 31) + this.fN) * 31) + kc.hashCode(this.fO)) * 31) + this.fP) * 31) + this.fQ) * 31;
            int hashCode2 = (this.aae == null || this.aae.isEmpty()) ? 0 : this.aae.hashCode();
            return hashCode2 + hashCode;
        }

        public h i(jy jyVar) throws IOException {
            while (true) {
                int ky = jyVar.ky();
                int c;
                Object obj;
                int cw;
                Object obj2;
                switch (ky) {
                    case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                        break;
                    case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                        c = kh.c(jyVar, 8);
                        ky = this.fK == null ? 0 : this.fK.length;
                        obj = new int[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fK, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.kB();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.kB();
                        this.fK = obj;
                        continue;
                    case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                        cw = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            jyVar.kB();
                            ky++;
                        }
                        jyVar.cy(c);
                        c = this.fK == null ? 0 : this.fK.length;
                        obj2 = new int[(ky + c)];
                        if (c != 0) {
                            System.arraycopy(this.fK, 0, obj2, 0, c);
                        }
                        while (c < obj2.length) {
                            obj2[c] = jyVar.kB();
                            c++;
                        }
                        this.fK = obj2;
                        jyVar.cx(cw);
                        continue;
                    case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                        c = kh.c(jyVar, 16);
                        ky = this.fL == null ? 0 : this.fL.length;
                        obj = new int[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fL, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.kB();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.kB();
                        this.fL = obj;
                        continue;
                    case 18:
                        cw = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            jyVar.kB();
                            ky++;
                        }
                        jyVar.cy(c);
                        c = this.fL == null ? 0 : this.fL.length;
                        obj2 = new int[(ky + c)];
                        if (c != 0) {
                            System.arraycopy(this.fL, 0, obj2, 0, c);
                        }
                        while (c < obj2.length) {
                            obj2[c] = jyVar.kB();
                            c++;
                        }
                        this.fL = obj2;
                        jyVar.cx(cw);
                        continue;
                    case 24:
                        c = kh.c(jyVar, 24);
                        ky = this.fM == null ? 0 : this.fM.length;
                        obj = new int[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fM, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.kB();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.kB();
                        this.fM = obj;
                        continue;
                    case 26:
                        cw = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            jyVar.kB();
                            ky++;
                        }
                        jyVar.cy(c);
                        c = this.fM == null ? 0 : this.fM.length;
                        obj2 = new int[(ky + c)];
                        if (c != 0) {
                            System.arraycopy(this.fM, 0, obj2, 0, c);
                        }
                        while (c < obj2.length) {
                            obj2[c] = jyVar.kB();
                            c++;
                        }
                        this.fM = obj2;
                        jyVar.cx(cw);
                        continue;
                    case TransportMediator.FLAG_KEY_MEDIA_STOP /*32*/:
                        this.fN = jyVar.kB();
                        continue;
                    case 40:
                        c = kh.c(jyVar, 40);
                        ky = this.fO == null ? 0 : this.fO.length;
                        obj = new int[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fO, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = jyVar.kB();
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = jyVar.kB();
                        this.fO = obj;
                        continue;
                    case 42:
                        cw = jyVar.cw(jyVar.kE());
                        c = jyVar.getPosition();
                        ky = 0;
                        while (jyVar.kJ() > 0) {
                            jyVar.kB();
                            ky++;
                        }
                        jyVar.cy(c);
                        c = this.fO == null ? 0 : this.fO.length;
                        obj2 = new int[(ky + c)];
                        if (c != 0) {
                            System.arraycopy(this.fO, 0, obj2, 0, c);
                        }
                        while (c < obj2.length) {
                            obj2[c] = jyVar.kB();
                            c++;
                        }
                        this.fO = obj2;
                        jyVar.cx(cw);
                        continue;
                    case 48:
                        this.fP = jyVar.kB();
                        continue;
                    case 56:
                        this.fQ = jyVar.kB();
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

        public h n() {
            this.fK = kh.aaj;
            this.fL = kh.aaj;
            this.fM = kh.aaj;
            this.fN = 0;
            this.fO = kh.aaj;
            this.fP = 0;
            this.fQ = 0;
            this.aae = null;
            this.DY = -1;
            return this;
        }
    }

    public static final class i extends ka<i> {
        private static volatile i[] fR;
        public com.google.android.gms.internal.d.a fS;
        public d fT;
        public String name;

        public i() {
            p();
        }

        public static i[] o() {
            if (fR == null) {
                synchronized (kc.aah) {
                    if (fR == null) {
                        fR = new i[0];
                    }
                }
            }
            return fR;
        }

        public void a(jz jzVar) throws IOException {
            if (!this.name.equals("")) {
                jzVar.b(1, this.name);
            }
            if (this.fS != null) {
                jzVar.a(2, this.fS);
            }
            if (this.fT != null) {
                jzVar.a(3, this.fT);
            }
            super.a(jzVar);
        }

        public /* synthetic */ ke b(jy jyVar) throws IOException {
            return j(jyVar);
        }

        public int c() {
            int c = super.c();
            if (!this.name.equals("")) {
                c += jz.g(1, this.name);
            }
            if (this.fS != null) {
                c += jz.b(2, this.fS);
            }
            if (this.fT != null) {
                c += jz.b(3, this.fT);
            }
            this.DY = c;
            return c;
        }

        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof i)) {
                return false;
            }
            i iVar = (i) o;
            if (this.name == null) {
                if (iVar.name != null) {
                    return false;
                }
            } else if (!this.name.equals(iVar.name)) {
                return false;
            }
            if (this.fS == null) {
                if (iVar.fS != null) {
                    return false;
                }
            } else if (!this.fS.equals(iVar.fS)) {
                return false;
            }
            if (this.fT == null) {
                if (iVar.fT != null) {
                    return false;
                }
            } else if (!this.fT.equals(iVar.fT)) {
                return false;
            }
            if (this.aae == null || this.aae.isEmpty()) {
                return iVar.aae == null || iVar.aae.isEmpty();
            } else {
                return this.aae.equals(iVar.aae);
            }
        }

        public int hashCode() {
            int i = 0;
            int hashCode = ((this.fT == null ? 0 : this.fT.hashCode()) + (((this.fS == null ? 0 : this.fS.hashCode()) + (((this.name == null ? 0 : this.name.hashCode()) + 527) * 31)) * 31)) * 31;
            if (!(this.aae == null || this.aae.isEmpty())) {
                i = this.aae.hashCode();
            }
            return hashCode + i;
        }

        public i j(jy jyVar) throws IOException {
            while (true) {
                int ky = jyVar.ky();
                switch (ky) {
                    case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                        break;
                    case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                        this.name = jyVar.readString();
                        continue;
                    case 18:
                        if (this.fS == null) {
                            this.fS = new com.google.android.gms.internal.d.a();
                        }
                        jyVar.a(this.fS);
                        continue;
                    case 26:
                        if (this.fT == null) {
                            this.fT = new d();
                        }
                        jyVar.a(this.fT);
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

        public i p() {
            this.name = "";
            this.fS = null;
            this.fT = null;
            this.aae = null;
            this.DY = -1;
            return this;
        }
    }

    public static final class j extends ka<j> {
        public i[] fU;
        public f fV;
        public String fW;

        public j() {
            q();
        }

        public static j b(byte[] bArr) throws kd {
            return (j) ke.a(new j(), bArr);
        }

        public void a(jz jzVar) throws IOException {
            if (this.fU != null && this.fU.length > 0) {
                for (ke keVar : this.fU) {
                    if (keVar != null) {
                        jzVar.a(1, keVar);
                    }
                }
            }
            if (this.fV != null) {
                jzVar.a(2, this.fV);
            }
            if (!this.fW.equals("")) {
                jzVar.b(3, this.fW);
            }
            super.a(jzVar);
        }

        public /* synthetic */ ke b(jy jyVar) throws IOException {
            return k(jyVar);
        }

        public int c() {
            int c = super.c();
            if (this.fU != null && this.fU.length > 0) {
                for (ke keVar : this.fU) {
                    if (keVar != null) {
                        c += jz.b(1, keVar);
                    }
                }
            }
            if (this.fV != null) {
                c += jz.b(2, this.fV);
            }
            if (!this.fW.equals("")) {
                c += jz.g(3, this.fW);
            }
            this.DY = c;
            return c;
        }

        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof j)) {
                return false;
            }
            j jVar = (j) o;
            if (!kc.equals(this.fU, jVar.fU)) {
                return false;
            }
            if (this.fV == null) {
                if (jVar.fV != null) {
                    return false;
                }
            } else if (!this.fV.equals(jVar.fV)) {
                return false;
            }
            if (this.fW == null) {
                if (jVar.fW != null) {
                    return false;
                }
            } else if (!this.fW.equals(jVar.fW)) {
                return false;
            }
            if (this.aae == null || this.aae.isEmpty()) {
                return jVar.aae == null || jVar.aae.isEmpty();
            } else {
                return this.aae.equals(jVar.aae);
            }
        }

        public int hashCode() {
            int i = 0;
            int hashCode = ((this.fW == null ? 0 : this.fW.hashCode()) + (((this.fV == null ? 0 : this.fV.hashCode()) + ((kc.hashCode(this.fU) + 527) * 31)) * 31)) * 31;
            if (!(this.aae == null || this.aae.isEmpty())) {
                i = this.aae.hashCode();
            }
            return hashCode + i;
        }

        public j k(jy jyVar) throws IOException {
            while (true) {
                int ky = jyVar.ky();
                switch (ky) {
                    case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                        break;
                    case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                        int c = kh.c(jyVar, 10);
                        ky = this.fU == null ? 0 : this.fU.length;
                        Object obj = new i[(c + ky)];
                        if (ky != 0) {
                            System.arraycopy(this.fU, 0, obj, 0, ky);
                        }
                        while (ky < obj.length - 1) {
                            obj[ky] = new i();
                            jyVar.a(obj[ky]);
                            jyVar.ky();
                            ky++;
                        }
                        obj[ky] = new i();
                        jyVar.a(obj[ky]);
                        this.fU = obj;
                        continue;
                    case 18:
                        if (this.fV == null) {
                            this.fV = new f();
                        }
                        jyVar.a(this.fV);
                        continue;
                    case 26:
                        this.fW = jyVar.readString();
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

        public j q() {
            this.fU = i.o();
            this.fV = null;
            this.fW = "";
            this.aae = null;
            this.DY = -1;
            return this;
        }
    }
}
