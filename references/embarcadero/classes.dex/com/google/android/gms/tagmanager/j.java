package com.google.android.gms.tagmanager;

import android.support.v4.view.MotionEventCompat;

class j {
    public static byte[] aX(String str) {
        int length = str.length();
        if (length % 2 != 0) {
            throw new IllegalArgumentException("purported base16 string has odd number of characters");
        }
        byte[] bArr = new byte[(length / 2)];
        for (int i = 0; i < length; i += 2) {
            int digit = Character.digit(str.charAt(i), 16);
            int digit2 = Character.digit(str.charAt(i + 1), 16);
            if (digit == -1 || digit2 == -1) {
                throw new IllegalArgumentException("purported base16 string has illegal char");
            }
            bArr[i / 2] = (byte) ((digit << 4) + digit2);
        }
        return bArr;
    }

    public static String d(byte[] bArr) {
        StringBuilder stringBuilder = new StringBuilder();
        for (byte b : bArr) {
            if ((b & 240) == 0) {
                stringBuilder.append("0");
            }
            stringBuilder.append(Integer.toHexString(b & MotionEventCompat.ACTION_MASK));
        }
        return stringBuilder.toString().toUpperCase();
    }
}
