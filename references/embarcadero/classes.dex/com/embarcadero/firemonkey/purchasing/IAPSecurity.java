package com.embarcadero.firemonkey.purchasing;

import android.util.Base64;
import java.security.InvalidKeyException;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.X509EncodedKeySpec;

public class IAPSecurity {
    private static final String TAG = "IAPSecurity";
    private static String keyFactoryAlgorithm;
    private static String signatureAlgorithm;

    static {
        keyFactoryAlgorithm = "RSA";
        signatureAlgorithm = "SHA1withRSA";
    }

    private static PublicKey generatePublicKey(String base64EncodedPublicKey) {
        PublicKey publicKey = null;
        try {
            publicKey = KeyFactory.getInstance(keyFactoryAlgorithm).generatePublic(new X509EncodedKeySpec(Base64.decode(base64EncodedPublicKey, 0)));
        } catch (NoSuchAlgorithmException e) {
        } catch (InvalidKeySpecException e2) {
        } catch (IllegalArgumentException e3) {
        }
        return publicKey;
    }

    private static boolean verify(PublicKey publicKey, String signedData, String signature) {
        try {
            Signature sig = Signature.getInstance(signatureAlgorithm);
            sig.initVerify(publicKey);
            sig.update(signedData.getBytes());
            if (sig.verify(Base64.decode(signature, 0))) {
                return true;
            }
            return false;
        } catch (NoSuchAlgorithmException e) {
            return false;
        } catch (InvalidKeyException e2) {
            return false;
        } catch (SignatureException e3) {
            return false;
        } catch (IllegalArgumentException e4) {
            return false;
        }
    }

    public static boolean verifyPurchase(String base64PublicKey, String signedData, String signature) {
        if (base64PublicKey == null || signedData == null || signature == null) {
            return false;
        }
        return verify(generatePublicKey(base64PublicKey), signedData, signature);
    }
}
