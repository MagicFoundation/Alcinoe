package com.google.android.gms.internal;

import java.nio.ByteBuffer;
import java.security.Key;
import java.security.SecureRandom;
import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

public class p {
    private final n ka;
    private final SecureRandom kt;

    public class a extends Exception {
        final /* synthetic */ p ku;

        public a(p pVar) {
            this.ku = pVar;
        }

        public a(p pVar, Throwable th) {
            this.ku = pVar;
            super(th);
        }
    }

    public p(n nVar, SecureRandom secureRandom) {
        this.ka = nVar;
        this.kt = secureRandom;
    }

    static void c(byte[] bArr) {
        for (int i = 0; i < bArr.length; i++) {
            bArr[i] = (byte) (bArr[i] ^ 68);
        }
    }

    public byte[] c(byte[] bArr, String str) throws a {
        if (bArr.length != 16) {
            throw new a(this);
        }
        try {
            byte[] a = this.ka.a(str, false);
            if (a.length <= 16) {
                throw new a(this);
            }
            ByteBuffer allocate = ByteBuffer.allocate(a.length);
            allocate.put(a);
            allocate.flip();
            byte[] bArr2 = new byte[16];
            a = new byte[(a.length - 16)];
            allocate.get(bArr2);
            allocate.get(a);
            Key secretKeySpec = new SecretKeySpec(bArr, "AES");
            Cipher instance = Cipher.getInstance("AES/CBC/PKCS5Padding");
            instance.init(2, secretKeySpec, new IvParameterSpec(bArr2));
            return instance.doFinal(a);
        } catch (Throwable e) {
            throw new a(this, e);
        } catch (Throwable e2) {
            throw new a(this, e2);
        } catch (Throwable e22) {
            throw new a(this, e22);
        } catch (Throwable e222) {
            throw new a(this, e222);
        } catch (Throwable e2222) {
            throw new a(this, e2222);
        } catch (Throwable e22222) {
            throw new a(this, e22222);
        } catch (Throwable e222222) {
            throw new a(this, e222222);
        }
    }

    public byte[] d(String str) throws a {
        try {
            byte[] a = this.ka.a(str, false);
            if (a.length != 32) {
                throw new a(this);
            }
            byte[] bArr = new byte[16];
            ByteBuffer.wrap(a, 4, 16).get(bArr);
            c(bArr);
            return bArr;
        } catch (Throwable e) {
            throw new a(this, e);
        }
    }
}
