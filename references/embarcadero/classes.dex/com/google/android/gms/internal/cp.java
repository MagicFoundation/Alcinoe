package com.google.android.gms.internal;

import android.content.Context;
import android.os.Bundle;
import com.google.android.gms.internal.v.a;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.UUID;

public final class cp {
    private static final Object op;
    public static final String pu;
    private static cq pv;
    private static BigInteger pw;
    private static HashSet<co> px;
    private static HashMap<String, cr> py;

    static {
        UUID randomUUID = UUID.randomUUID();
        byte[] toByteArray = BigInteger.valueOf(randomUUID.getLeastSignificantBits()).toByteArray();
        byte[] toByteArray2 = BigInteger.valueOf(randomUUID.getMostSignificantBits()).toByteArray();
        String bigInteger = new BigInteger(1, toByteArray).toString();
        for (int i = 0; i < 2; i++) {
            try {
                MessageDigest instance = MessageDigest.getInstance("MD5");
                instance.update(toByteArray);
                instance.update(toByteArray2);
                Object obj = new byte[8];
                System.arraycopy(instance.digest(), 0, obj, 0, 8);
                bigInteger = new BigInteger(1, obj).toString();
            } catch (NoSuchAlgorithmException e) {
            }
        }
        pu = bigInteger;
        op = new Object();
        pv = new cq(pu);
        pw = BigInteger.ONE;
        px = new HashSet();
        py = new HashMap();
    }

    public static Bundle a(a aVar, String str, Context context) {
        Bundle bundle;
        synchronized (op) {
            bundle = new Bundle();
            bundle.putBundle("app", pv.b(str, context));
            Bundle bundle2 = new Bundle();
            for (String str2 : py.keySet()) {
                bundle2.putBundle(str2, ((cr) py.get(str2)).toBundle());
            }
            bundle.putBundle("slots", bundle2);
            ArrayList arrayList = new ArrayList();
            Iterator it = px.iterator();
            while (it.hasNext()) {
                arrayList.add(((co) it.next()).toBundle());
            }
            bundle.putParcelableArrayList("ads", arrayList);
            aVar.a(px);
            px = new HashSet();
        }
        return bundle;
    }

    public static void a(co coVar) {
        synchronized (op) {
            px.add(coVar);
        }
    }

    public static void a(a aVar) {
        synchronized (op) {
            px.addAll(aVar.ah());
        }
    }

    public static void a(String str, cr crVar) {
        synchronized (op) {
            py.put(str, crVar);
        }
    }

    public static String aP() {
        String bigInteger;
        synchronized (op) {
            bigInteger = pw.toString();
            pw = pw.add(BigInteger.ONE);
        }
        return bigInteger;
    }

    public static cq aQ() {
        cq cqVar;
        synchronized (op) {
            cqVar = pv;
        }
        return cqVar;
    }
}
