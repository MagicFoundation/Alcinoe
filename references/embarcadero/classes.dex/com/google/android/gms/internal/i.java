package com.google.android.gms.internal;

import android.content.Context;
import android.util.DisplayMetrics;
import android.view.MotionEvent;
import com.google.android.vending.licensing.Policy;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;

public abstract class i implements h {
    protected MotionEvent jY;
    protected DisplayMetrics jZ;
    protected n ka;
    private o kb;

    protected i(Context context, n nVar, o oVar) {
        this.ka = nVar;
        this.kb = oVar;
        try {
            this.jZ = context.getResources().getDisplayMetrics();
        } catch (UnsupportedOperationException e) {
            this.jZ = new DisplayMetrics();
            this.jZ.density = 1.0f;
        }
    }

    private String a(Context context, String str, boolean z) {
        try {
            byte[] u;
            synchronized (this) {
                t();
                if (z) {
                    c(context);
                } else {
                    b(context);
                }
                u = u();
            }
            return u.length == 0 ? Integer.toString(5) : a(u, str);
        } catch (NoSuchAlgorithmException e) {
            return Integer.toString(7);
        } catch (UnsupportedEncodingException e2) {
            return Integer.toString(7);
        } catch (IOException e3) {
            return Integer.toString(3);
        }
    }

    private void t() {
        this.kb.reset();
    }

    private byte[] u() throws IOException {
        return this.kb.z();
    }

    public String a(Context context) {
        return a(context, null, false);
    }

    public String a(Context context, String str) {
        return a(context, str, true);
    }

    String a(byte[] bArr, String str) throws NoSuchAlgorithmException, UnsupportedEncodingException, IOException {
        byte[] bArr2;
        if (bArr.length > 239) {
            t();
            a(20, 1);
            bArr = u();
        }
        if (bArr.length < 239) {
            bArr2 = new byte[(239 - bArr.length)];
            new SecureRandom().nextBytes(bArr2);
            bArr2 = ByteBuffer.allocate(240).put((byte) bArr.length).put(bArr).put(bArr2).array();
        } else {
            bArr2 = ByteBuffer.allocate(240).put((byte) bArr.length).put(bArr).array();
        }
        MessageDigest instance = MessageDigest.getInstance("MD5");
        instance.update(bArr2);
        bArr2 = ByteBuffer.allocate(Policy.LICENSED).put(instance.digest()).put(bArr2).array();
        byte[] bArr3 = new byte[Policy.LICENSED];
        new f().a(bArr2, bArr3);
        if (str != null && str.length() > 0) {
            a(str, bArr3);
        }
        return this.ka.a(bArr3, true);
    }

    public void a(int i, int i2, int i3) {
        if (this.jY != null) {
            this.jY.recycle();
        }
        this.jY = MotionEvent.obtain(0, (long) i3, 1, ((float) i) * this.jZ.density, ((float) i2) * this.jZ.density, 0.0f, 0.0f, 0, 0.0f, 0.0f, 0, 0);
    }

    protected void a(int i, long j) throws IOException {
        this.kb.b(i, j);
    }

    protected void a(int i, String str) throws IOException {
        this.kb.b(i, str);
    }

    public void a(MotionEvent motionEvent) {
        if (motionEvent.getAction() == 1) {
            if (this.jY != null) {
                this.jY.recycle();
            }
            this.jY = MotionEvent.obtain(motionEvent);
        }
    }

    void a(String str, byte[] bArr) throws UnsupportedEncodingException {
        if (str.length() > 32) {
            str = str.substring(0, 32);
        }
        new jx(str.getBytes("UTF-8")).m(bArr);
    }

    protected abstract void b(Context context);

    protected abstract void c(Context context);
}
