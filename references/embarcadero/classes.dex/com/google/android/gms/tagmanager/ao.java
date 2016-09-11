package com.google.android.gms.tagmanager;

import com.google.android.gms.internal.a;
import com.google.android.gms.internal.b;
import com.google.android.gms.internal.d;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Map;

class ao extends aj {
    private static final String ID;
    private static final String US;
    private static final String UU;
    private static final String UY;

    static {
        ID = a.HASH.toString();
        US = b.ARG0.toString();
        UY = b.ALGORITHM.toString();
        UU = b.INPUT_FORMAT.toString();
    }

    public ao() {
        super(ID, US);
    }

    private byte[] c(String str, byte[] bArr) throws NoSuchAlgorithmException {
        MessageDigest instance = MessageDigest.getInstance(str);
        instance.update(bArr);
        return instance.digest();
    }

    public boolean iy() {
        return true;
    }

    public d.a u(Map<String, d.a> map) {
        d.a aVar = (d.a) map.get(US);
        if (aVar == null || aVar == di.ku()) {
            return di.ku();
        }
        byte[] bytes;
        String j = di.j(aVar);
        aVar = (d.a) map.get(UY);
        String j2 = aVar == null ? "MD5" : di.j(aVar);
        aVar = (d.a) map.get(UU);
        String j3 = aVar == null ? "text" : di.j(aVar);
        if ("text".equals(j3)) {
            bytes = j.getBytes();
        } else if ("base16".equals(j3)) {
            bytes = j.aX(j);
        } else {
            bh.t("Hash: unknown input format: " + j3);
            return di.ku();
        }
        try {
            return di.r(j.d(c(j2, bytes)));
        } catch (NoSuchAlgorithmException e) {
            bh.t("Hash: unknown algorithm: " + j2);
            return di.ku();
        }
    }
}
