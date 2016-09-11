package com.google.android.gms.internal;

import java.io.IOException;
import java.util.concurrent.atomic.AtomicInteger;

public abstract class dp {
    private static final AtomicInteger xA;
    protected final du xB;
    private final String xC;
    private dw xD;

    static {
        xA = new AtomicInteger(0);
    }

    protected dp(String str, String str2) {
        this.xC = str;
        this.xB = new du(str2);
        this.xB.U(String.format("instance-%d", new Object[]{Integer.valueOf(xA.incrementAndGet())}));
    }

    public void P(String str) {
    }

    public void a(long j, int i) {
    }

    public final void a(dw dwVar) {
        this.xD = dwVar;
        if (this.xD == null) {
            cX();
        }
    }

    protected final void a(String str, long j, String str2) throws IOException {
        this.xB.a("Sending text message: %s to: %s", str, str2);
        this.xD.a(this.xC, str, j, str2);
    }

    protected final long cW() {
        return this.xD.cV();
    }

    public void cX() {
    }

    public final String getNamespace() {
        return this.xC;
    }
}
