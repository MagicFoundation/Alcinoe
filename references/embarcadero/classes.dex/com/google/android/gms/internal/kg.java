package com.google.android.gms.internal;

import java.util.Arrays;

public final class kg {
    final byte[] aai;
    final int tag;

    kg(int i, byte[] bArr) {
        this.tag = i;
        this.aai = bArr;
    }

    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof kg)) {
            return false;
        }
        kg kgVar = (kg) o;
        return this.tag == kgVar.tag && Arrays.equals(this.aai, kgVar.aai);
    }

    public int hashCode() {
        return ((this.tag + 527) * 31) + Arrays.hashCode(this.aai);
    }
}
