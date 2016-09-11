package com.google.android.gms.internal;

import java.util.Arrays;

public final class kc {
    public static final Object aah;

    static {
        aah = new Object();
    }

    public static boolean equals(int[] field1, int[] field2) {
        if (field1 == null || field1.length == 0) {
            return field2 == null || field2.length == 0;
        } else {
            return Arrays.equals(field1, field2);
        }
    }

    public static boolean equals(Object[] field1, Object[] field2) {
        boolean length = field1 == null ? false : field1.length;
        int length2 = field2 == null ? 0 : field2.length;
        int i = 0;
        boolean z = false;
        while (true) {
            if (z >= length || field1[z] != null) {
                int i2 = i;
                while (i2 < length2 && field2[i2] == null) {
                    i2++;
                }
                boolean z2 = z >= length;
                boolean z3 = i2 >= length2;
                if (z2 && z3) {
                    return true;
                }
                if (z2 != z3 || !field1[z].equals(field2[i2])) {
                    return false;
                }
                i = i2 + 1;
                z++;
            } else {
                z++;
            }
        }
    }

    public static int hashCode(int[] field) {
        return (field == null || field.length == 0) ? 0 : Arrays.hashCode(field);
    }

    public static int hashCode(Object[] field) {
        int i = 0;
        int length = field == null ? 0 : field.length;
        for (int i2 = 0; i2 < length; i2++) {
            Object obj = field[i2];
            if (obj != null) {
                i = (i * 31) + obj.hashCode();
            }
        }
        return i;
    }
}
