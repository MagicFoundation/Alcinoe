package com.google.android.gms.internal;

import java.io.IOException;

public final class kh {
    public static final int[] aaj;
    public static final long[] aak;
    public static final float[] aal;
    public static final double[] aam;
    public static final boolean[] aan;
    public static final String[] aao;
    public static final byte[][] aap;
    public static final byte[] aaq;

    static {
        aaj = new int[0];
        aak = new long[0];
        aal = new float[0];
        aam = new double[0];
        aan = new boolean[0];
        aao = new String[0];
        aap = new byte[0][];
        aaq = new byte[0];
    }

    public static boolean b(jy jyVar, int i) throws IOException {
        return jyVar.cv(i);
    }

    public static final int c(jy jyVar, int i) throws IOException {
        int i2 = 1;
        int position = jyVar.getPosition();
        jyVar.cv(i);
        while (jyVar.kJ() > 0 && jyVar.ky() == i) {
            jyVar.cv(i);
            i2++;
        }
        jyVar.cy(position);
        return i2;
    }

    static int cJ(int i) {
        return i & 7;
    }

    public static int cK(int i) {
        return i >>> 3;
    }

    static int i(int i, int i2) {
        return (i << 3) | i2;
    }
}
