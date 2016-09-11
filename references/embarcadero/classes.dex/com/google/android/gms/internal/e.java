package com.google.android.gms.internal;

import android.util.Base64;

class e implements n {
    e() {
    }

    public String a(byte[] bArr, boolean z) {
        return Base64.encodeToString(bArr, z ? 11 : 2);
    }

    public byte[] a(String str, boolean z) throws IllegalArgumentException {
        return Base64.decode(str, z ? 11 : 2);
    }
}
