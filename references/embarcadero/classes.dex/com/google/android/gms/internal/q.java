package com.google.android.gms.internal;

import java.io.IOException;

class q implements o {
    private jz kv;
    private byte[] kw;
    private final int kx;

    public q(int i) {
        this.kx = i;
        reset();
    }

    public void b(int i, long j) throws IOException {
        this.kv.b(i, j);
    }

    public void b(int i, String str) throws IOException {
        this.kv.b(i, str);
    }

    public void reset() {
        this.kw = new byte[this.kx];
        this.kv = jz.o(this.kw);
    }

    public byte[] z() throws IOException {
        int kM = this.kv.kM();
        if (kM < 0) {
            throw new IOException();
        } else if (kM == 0) {
            return this.kw;
        } else {
            Object obj = new byte[(this.kw.length - kM)];
            System.arraycopy(this.kw, 0, obj, 0, obj.length);
            return obj;
        }
    }
}
