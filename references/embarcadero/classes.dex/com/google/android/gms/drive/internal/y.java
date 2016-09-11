package com.google.android.gms.drive.internal;

import android.support.v4.media.TransportMediator;
import com.google.android.gms.internal.jy;
import com.google.android.gms.internal.jz;
import com.google.android.gms.internal.kd;
import com.google.android.gms.internal.ke;
import com.google.android.gms.internal.kh;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.io.IOException;

public final class y extends ke {
    public static final y[] DU;
    public String DV;
    public long DW;
    public long DX;
    private int DY;
    public int versionCode;

    static {
        DU = new y[0];
    }

    public y() {
        this.versionCode = 1;
        this.DV = "";
        this.DW = -1;
        this.DX = -1;
        this.DY = -1;
    }

    public static y g(byte[] bArr) throws kd {
        return (y) ke.a(new y(), bArr);
    }

    public void a(jz jzVar) throws IOException {
        jzVar.f(1, this.versionCode);
        jzVar.b(2, this.DV);
        jzVar.c(3, this.DW);
        jzVar.c(4, this.DX);
    }

    public /* synthetic */ ke b(jy jyVar) throws IOException {
        return m(jyVar);
    }

    public int c() {
        int g = (((0 + jz.g(1, this.versionCode)) + jz.g(2, this.DV)) + jz.e(3, this.DW)) + jz.e(4, this.DX);
        this.DY = g;
        return g;
    }

    public int eW() {
        if (this.DY < 0) {
            c();
        }
        return this.DY;
    }

    public y m(jy jyVar) throws IOException {
        while (true) {
            int ky = jyVar.ky();
            switch (ky) {
                case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                    break;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    this.versionCode = jyVar.kB();
                    continue;
                case 18:
                    this.DV = jyVar.readString();
                    continue;
                case 24:
                    this.DW = jyVar.kD();
                    continue;
                case TransportMediator.FLAG_KEY_MEDIA_STOP /*32*/:
                    this.DX = jyVar.kD();
                    continue;
                default:
                    if (!kh.b(jyVar, ky)) {
                        break;
                    }
                    continue;
            }
            return this;
        }
    }
}
