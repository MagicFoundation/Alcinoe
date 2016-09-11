package com.google.android.gms.tagmanager;

import android.content.Context;
import java.net.URLEncoder;

class y implements aq {
    private static y UO;
    private static final Object qI;
    private String UP;
    private String UQ;
    private ar UR;
    private cg Uc;

    static {
        qI = new Object();
    }

    private y(Context context) {
        this(as.H(context), new cw());
    }

    y(ar arVar, cg cgVar) {
        this.UR = arVar;
        this.Uc = cgVar;
    }

    public static aq F(Context context) {
        aq aqVar;
        synchronized (qI) {
            if (UO == null) {
                UO = new y(context);
            }
            aqVar = UO;
        }
        return aqVar;
    }

    public boolean bk(String str) {
        if (this.Uc.cl()) {
            if (!(this.UP == null || this.UQ == null)) {
                try {
                    str = this.UP + "?" + this.UQ + "=" + URLEncoder.encode(str, "UTF-8");
                    bh.v("Sending wrapped url hit: " + str);
                } catch (Throwable e) {
                    bh.b("Error wrapping URL for testing.", e);
                    return false;
                }
            }
            this.UR.bn(str);
            return true;
        }
        bh.w("Too many urls sent too quickly with the TagManagerSender, rate limiting invoked.");
        return false;
    }
}
