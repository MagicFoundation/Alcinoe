package com.google.android.gms.internal;

import android.support.v4.media.TransportMediator;
import android.support.v4.view.MotionEventCompat;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.io.IOException;

public final class jy {
    private int ZU;
    private int ZV;
    private int ZW;
    private int ZX;
    private int ZY;
    private int ZZ;
    private int aaa;
    private int aab;
    private int aac;
    private final byte[] buffer;

    private jy(byte[] bArr, int i, int i2) {
        this.ZZ = Integer.MAX_VALUE;
        this.aab = 64;
        this.aac = 67108864;
        this.buffer = bArr;
        this.ZU = i;
        this.ZV = i + i2;
        this.ZX = i;
    }

    public static jy a(byte[] bArr, int i, int i2) {
        return new jy(bArr, i, i2);
    }

    private void kI() {
        this.ZV += this.ZW;
        int i = this.ZV;
        if (i > this.ZZ) {
            this.ZW = i - this.ZZ;
            this.ZV -= this.ZW;
            return;
        }
        this.ZW = 0;
    }

    public static jy n(byte[] bArr) {
        return a(bArr, 0, bArr.length);
    }

    public static long w(long j) {
        return (j >>> 1) ^ (-(1 & j));
    }

    public void a(ke keVar) throws IOException {
        int kE = kE();
        if (this.aaa >= this.aab) {
            throw kd.kU();
        }
        kE = cw(kE);
        this.aaa++;
        keVar.b(this);
        cu(0);
        this.aaa--;
        cx(kE);
    }

    public void a(ke keVar, int i) throws IOException {
        if (this.aaa >= this.aab) {
            throw kd.kU();
        }
        this.aaa++;
        keVar.b(this);
        cu(kh.i(i, 4));
        this.aaa--;
    }

    public void cA(int i) throws IOException {
        if (i < 0) {
            throw kd.kP();
        } else if (this.ZX + i > this.ZZ) {
            cA(this.ZZ - this.ZX);
            throw kd.kO();
        } else if (i <= this.ZV - this.ZX) {
            this.ZX += i;
        } else {
            throw kd.kO();
        }
    }

    public void cu(int i) throws kd {
        if (this.ZY != i) {
            throw kd.kS();
        }
    }

    public boolean cv(int i) throws IOException {
        switch (kh.cJ(i)) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                kB();
                return true;
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                kH();
                return true;
            case DetectedActivity.ON_FOOT /*2*/:
                cA(kE());
                return true;
            case DetectedActivity.STILL /*3*/:
                kz();
                cu(kh.i(kh.cK(i), 4));
                return true;
            case DetectedActivity.UNKNOWN /*4*/:
                return false;
            case DetectedActivity.TILTING /*5*/:
                kG();
                return true;
            default:
                throw kd.kT();
        }
    }

    public int cw(int i) throws kd {
        if (i < 0) {
            throw kd.kP();
        }
        int i2 = this.ZX + i;
        int i3 = this.ZZ;
        if (i2 > i3) {
            throw kd.kO();
        }
        this.ZZ = i2;
        kI();
        return i3;
    }

    public void cx(int i) {
        this.ZZ = i;
        kI();
    }

    public void cy(int i) {
        if (i > this.ZX - this.ZU) {
            throw new IllegalArgumentException("Position " + i + " is beyond current " + (this.ZX - this.ZU));
        } else if (i < 0) {
            throw new IllegalArgumentException("Bad position " + i);
        } else {
            this.ZX = this.ZU + i;
        }
    }

    public byte[] cz(int i) throws IOException {
        if (i < 0) {
            throw kd.kP();
        } else if (this.ZX + i > this.ZZ) {
            cA(this.ZZ - this.ZX);
            throw kd.kO();
        } else if (i <= this.ZV - this.ZX) {
            Object obj = new byte[i];
            System.arraycopy(this.buffer, this.ZX, obj, 0, i);
            this.ZX += i;
            return obj;
        } else {
            throw kd.kO();
        }
    }

    public byte[] e(int i, int i2) {
        if (i2 == 0) {
            return kh.aaq;
        }
        Object obj = new byte[i2];
        System.arraycopy(this.buffer, this.ZU + i, obj, 0, i2);
        return obj;
    }

    public int getPosition() {
        return this.ZX - this.ZU;
    }

    public long kA() throws IOException {
        return kF();
    }

    public int kB() throws IOException {
        return kE();
    }

    public boolean kC() throws IOException {
        return kE() != 0;
    }

    public long kD() throws IOException {
        return w(kF());
    }

    public int kE() throws IOException {
        byte kL = kL();
        if (kL >= null) {
            return kL;
        }
        int i = kL & TransportMediator.KEYCODE_MEDIA_PAUSE;
        byte kL2 = kL();
        if (kL2 >= null) {
            return i | (kL2 << 7);
        }
        i |= (kL2 & TransportMediator.KEYCODE_MEDIA_PAUSE) << 7;
        kL2 = kL();
        if (kL2 >= null) {
            return i | (kL2 << 14);
        }
        i |= (kL2 & TransportMediator.KEYCODE_MEDIA_PAUSE) << 14;
        kL2 = kL();
        if (kL2 >= null) {
            return i | (kL2 << 21);
        }
        i |= (kL2 & TransportMediator.KEYCODE_MEDIA_PAUSE) << 21;
        kL2 = kL();
        i |= kL2 << 28;
        if (kL2 >= null) {
            return i;
        }
        for (int i2 = 0; i2 < 5; i2++) {
            if (kL() >= null) {
                return i;
            }
        }
        throw kd.kQ();
    }

    public long kF() throws IOException {
        long j = 0;
        for (int i = 0; i < 64; i += 7) {
            byte kL = kL();
            j |= ((long) (kL & TransportMediator.KEYCODE_MEDIA_PAUSE)) << i;
            if ((kL & TransportMediator.FLAG_KEY_MEDIA_NEXT) == 0) {
                return j;
            }
        }
        throw kd.kQ();
    }

    public int kG() throws IOException {
        return (((kL() & MotionEventCompat.ACTION_MASK) | ((kL() & MotionEventCompat.ACTION_MASK) << 8)) | ((kL() & MotionEventCompat.ACTION_MASK) << 16)) | ((kL() & MotionEventCompat.ACTION_MASK) << 24);
    }

    public long kH() throws IOException {
        byte kL = kL();
        byte kL2 = kL();
        return ((((((((((long) kL2) & 255) << 8) | (((long) kL) & 255)) | ((((long) kL()) & 255) << 16)) | ((((long) kL()) & 255) << 24)) | ((((long) kL()) & 255) << 32)) | ((((long) kL()) & 255) << 40)) | ((((long) kL()) & 255) << 48)) | ((((long) kL()) & 255) << 56);
    }

    public int kJ() {
        if (this.ZZ == Integer.MAX_VALUE) {
            return -1;
        }
        return this.ZZ - this.ZX;
    }

    public boolean kK() {
        return this.ZX == this.ZV;
    }

    public byte kL() throws IOException {
        if (this.ZX == this.ZV) {
            throw kd.kO();
        }
        byte[] bArr = this.buffer;
        int i = this.ZX;
        this.ZX = i + 1;
        return bArr[i];
    }

    public int ky() throws IOException {
        if (kK()) {
            this.ZY = 0;
            return 0;
        }
        this.ZY = kE();
        if (this.ZY != 0) {
            return this.ZY;
        }
        throw kd.kR();
    }

    public void kz() throws IOException {
        int ky;
        do {
            ky = ky();
            if (ky == 0) {
                return;
            }
        } while (cv(ky));
    }

    public float readFloat() throws IOException {
        return Float.intBitsToFloat(kG());
    }

    public String readString() throws IOException {
        int kE = kE();
        if (kE > this.ZV - this.ZX || kE <= 0) {
            return new String(cz(kE), "UTF-8");
        }
        String str = new String(this.buffer, this.ZX, kE, "UTF-8");
        this.ZX = kE + this.ZX;
        return str;
    }
}
