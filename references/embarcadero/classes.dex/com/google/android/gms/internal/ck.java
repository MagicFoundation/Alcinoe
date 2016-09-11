package com.google.android.gms.internal;

import com.google.android.gms.plus.PlusShare;
import java.util.Map;

public final class ck {
    private final Object mg;
    private int nX;
    private dd ng;
    public final ar oA;
    private String ox;
    private String oy;
    public final ar oz;

    public ck(String str) {
        this.mg = new Object();
        this.nX = -2;
        this.oz = new ar() {
            final /* synthetic */ ck oB;

            {
                this.oB = r1;
            }

            public void a(dd ddVar, Map<String, String> map) {
                synchronized (this.oB.mg) {
                    String str = (String) map.get("errors");
                    da.w("Invalid " + ((String) map.get("type")) + " request error: " + str);
                    this.oB.nX = 1;
                    this.oB.mg.notify();
                }
            }
        };
        this.oA = new ar() {
            final /* synthetic */ ck oB;

            {
                this.oB = r1;
            }

            public void a(dd ddVar, Map<String, String> map) {
                synchronized (this.oB.mg) {
                    String str = (String) map.get(PlusShare.KEY_CALL_TO_ACTION_URL);
                    if (str == null) {
                        da.w("URL missing in loadAdUrl GMSG.");
                        return;
                    }
                    if (str.contains("%40mediation_adapters%40")) {
                        str = str.replaceAll("%40mediation_adapters%40", cs.b(ddVar.getContext(), (String) map.get("check_adapters"), this.oB.ox));
                        da.v("Ad request URL modified to " + str);
                    }
                    this.oB.oy = str;
                    this.oB.mg.notify();
                }
            }
        };
        this.ox = str;
    }

    /* JADX WARNING: inconsistent code. */
    /* Code decompiled incorrectly, please refer to instructions dump. */
    public String aI() {
        String str;
        synchronized (this.mg) {
            while (true) {
                if (this.oy == null && this.nX == -2) {
                    try {
                        this.mg.wait();
                    } catch (InterruptedException e) {
                        da.w("Ad request service was interrupted.");
                        str = null;
                    }
                } else {
                    str = this.oy;
                }
                return str;
            }
            str = this.oy;
        }
        return str;
    }

    public void b(dd ddVar) {
        synchronized (this.mg) {
            this.ng = ddVar;
        }
    }

    public int getErrorCode() {
        int i;
        synchronized (this.mg) {
            i = this.nX;
        }
        return i;
    }
}
