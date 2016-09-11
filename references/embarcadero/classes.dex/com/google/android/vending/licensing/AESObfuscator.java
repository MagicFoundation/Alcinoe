package com.google.android.vending.licensing;

import android.support.v4.view.accessibility.AccessibilityNodeInfoCompat;
import com.google.android.vending.licensing.util.Base64;
import com.google.android.vending.licensing.util.Base64DecoderException;
import java.io.UnsupportedEncodingException;
import java.security.GeneralSecurityException;
import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

public class AESObfuscator implements Obfuscator {
    private static final String CIPHER_ALGORITHM = "AES/CBC/PKCS5Padding";
    private static final byte[] IV;
    private static final String KEYGEN_ALGORITHM = "PBEWITHSHAAND256BITAES-CBC-BC";
    private static final String UTF8 = "UTF-8";
    private static final String header = "com.android.vending.licensing.AESObfuscator-1|";
    private Cipher mDecryptor;
    private Cipher mEncryptor;

    static {
        byte[] bArr = new byte[16];
        bArr[0] = (byte) 16;
        bArr[1] = (byte) 74;
        bArr[2] = (byte) 71;
        bArr[3] = (byte) -80;
        bArr[4] = (byte) 32;
        bArr[5] = (byte) 101;
        bArr[6] = (byte) -47;
        bArr[7] = (byte) 72;
        bArr[8] = (byte) 117;
        bArr[9] = (byte) -14;
        bArr[11] = (byte) -29;
        bArr[12] = (byte) 70;
        bArr[13] = (byte) 65;
        bArr[14] = (byte) -12;
        bArr[15] = (byte) 74;
        IV = bArr;
    }

    public AESObfuscator(byte[] salt, String applicationId, String deviceId) {
        try {
            SecretKey secret = new SecretKeySpec(SecretKeyFactory.getInstance(KEYGEN_ALGORITHM).generateSecret(new PBEKeySpec(new StringBuilder(String.valueOf(applicationId)).append(deviceId).toString().toCharArray(), salt, AccessibilityNodeInfoCompat.ACTION_NEXT_HTML_ELEMENT, Policy.LICENSED)).getEncoded(), "AES");
            this.mEncryptor = Cipher.getInstance(CIPHER_ALGORITHM);
            this.mEncryptor.init(1, secret, new IvParameterSpec(IV));
            this.mDecryptor = Cipher.getInstance(CIPHER_ALGORITHM);
            this.mDecryptor.init(2, secret, new IvParameterSpec(IV));
        } catch (GeneralSecurityException e) {
            throw new RuntimeException("Invalid environment", e);
        }
    }

    public String obfuscate(String original, String key) {
        if (original == null) {
            return null;
        }
        try {
            return Base64.encode(this.mEncryptor.doFinal(new StringBuilder(header).append(key).append(original).toString().getBytes(UTF8)));
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException("Invalid environment", e);
        } catch (GeneralSecurityException e2) {
            throw new RuntimeException("Invalid environment", e2);
        }
    }

    public String unobfuscate(String obfuscated, String key) throws ValidationException {
        if (obfuscated == null) {
            return null;
        }
        try {
            String result = new String(this.mDecryptor.doFinal(Base64.decode(obfuscated)), UTF8);
            if (result.indexOf(new StringBuilder(header).append(key).toString()) == 0) {
                return result.substring(header.length() + key.length(), result.length());
            }
            throw new ValidationException("Header not found (invalid data or key):" + obfuscated);
        } catch (Base64DecoderException e) {
            throw new ValidationException(e.getMessage() + ":" + obfuscated);
        } catch (IllegalBlockSizeException e2) {
            throw new ValidationException(e2.getMessage() + ":" + obfuscated);
        } catch (BadPaddingException e3) {
            throw new ValidationException(e3.getMessage() + ":" + obfuscated);
        } catch (UnsupportedEncodingException e4) {
            throw new RuntimeException("Invalid environment", e4);
        }
    }
}
