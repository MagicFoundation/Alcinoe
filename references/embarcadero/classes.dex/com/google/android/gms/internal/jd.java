package com.google.android.gms.internal;

import android.support.v4.media.TransportMediator;
import com.google.android.gms.internal.c.f;
import com.google.android.gms.internal.c.j;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.io.IOException;

public interface jd {

    public static final class a extends ka<a> {
        public long Yb;
        public j Yc;
        public f fV;

        public a() {
            kw();
        }

        public static a l(byte[] bArr) throws kd {
            return (a) ke.a(new a(), bArr);
        }

        public void a(jz jzVar) throws IOException {
            jzVar.b(1, this.Yb);
            if (this.fV != null) {
                jzVar.a(2, this.fV);
            }
            if (this.Yc != null) {
                jzVar.a(3, this.Yc);
            }
            super.a(jzVar);
        }

        public /* synthetic */ ke b(jy jyVar) throws IOException {
            return n(jyVar);
        }

        public int c() {
            int c = super.c() + jz.d(1, this.Yb);
            if (this.fV != null) {
                c += jz.b(2, this.fV);
            }
            if (this.Yc != null) {
                c += jz.b(3, this.Yc);
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
            if (this.Yb != aVar.Yb) {
                return false;
            }
            if (this.fV == null) {
                if (aVar.fV != null) {
                    return false;
                }
            } else if (!this.fV.equals(aVar.fV)) {
                return false;
            }
            if (this.Yc == null) {
                if (aVar.Yc != null) {
                    return false;
                }
            } else if (!this.Yc.equals(aVar.Yc)) {
                return false;
            }
            if (this.aae == null || this.aae.isEmpty()) {
                return aVar.aae == null || aVar.aae.isEmpty();
            } else {
                return this.aae.equals(aVar.aae);
            }
        }

        public int hashCode() {
            int i = 0;
            int hashCode = ((this.Yc == null ? 0 : this.Yc.hashCode()) + (((this.fV == null ? 0 : this.fV.hashCode()) + ((((int) (this.Yb ^ (this.Yb >>> 32))) + 527) * 31)) * 31)) * 31;
            if (!(this.aae == null || this.aae.isEmpty())) {
                i = this.aae.hashCode();
            }
            return hashCode + i;
        }

        public a kw() {
            this.Yb = 0;
            this.fV = null;
            this.Yc = null;
            this.aae = null;
            this.DY = -1;
            return this;
        }

        public a n(jy jyVar) throws IOException {
            while (true) {
                int ky = jyVar.ky();
                switch (ky) {
                    case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                        break;
                    case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                        this.Yb = jyVar.kA();
                        continue;
                    case 18:
                        if (this.fV == null) {
                            this.fV = new f();
                        }
                        jyVar.a(this.fV);
                        continue;
                    case 26:
                        if (this.Yc == null) {
                            this.Yc = new j();
                        }
                        jyVar.a(this.Yc);
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
    }
}
