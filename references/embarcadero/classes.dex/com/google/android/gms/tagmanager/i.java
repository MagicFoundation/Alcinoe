package com.google.android.gms.tagmanager;

import android.content.Context;
import android.net.Uri;
import android.net.Uri.Builder;
import com.google.android.gms.internal.b;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

class i extends dg {
    private static final String ID;
    private static final String TF;
    private static final String TG;
    static final String TH;
    private static final Set<String> TI;
    private static final String URL;
    private final a TJ;
    private final Context mContext;

    public interface a {
        aq iz();
    }

    /* renamed from: com.google.android.gms.tagmanager.i.1 */
    class AnonymousClass1 implements a {
        final /* synthetic */ Context os;

        AnonymousClass1(Context context) {
            this.os = context;
        }

        public aq iz() {
            return y.F(this.os);
        }
    }

    static {
        ID = com.google.android.gms.internal.a.ARBITRARY_PIXEL.toString();
        URL = b.URL.toString();
        TF = b.ADDITIONAL_PARAMS.toString();
        TG = b.UNREPEATABLE.toString();
        TH = "gtm_" + ID + "_unrepeatable";
        TI = new HashSet();
    }

    public i(Context context) {
        this(context, new AnonymousClass1(context));
    }

    i(Context context, a aVar) {
        super(ID, URL);
        this.TJ = aVar;
        this.mContext = context;
    }

    private synchronized boolean aU(String str) {
        boolean z = true;
        synchronized (this) {
            if (!aW(str)) {
                if (aV(str)) {
                    TI.add(str);
                } else {
                    z = false;
                }
            }
        }
        return z;
    }

    boolean aV(String str) {
        return this.mContext.getSharedPreferences(TH, 0).contains(str);
    }

    boolean aW(String str) {
        return TI.contains(str);
    }

    public void w(Map<String, com.google.android.gms.internal.d.a> map) {
        String j = map.get(TG) != null ? di.j((com.google.android.gms.internal.d.a) map.get(TG)) : null;
        if (j == null || !aU(j)) {
            Builder buildUpon = Uri.parse(di.j((com.google.android.gms.internal.d.a) map.get(URL))).buildUpon();
            com.google.android.gms.internal.d.a aVar = (com.google.android.gms.internal.d.a) map.get(TF);
            if (aVar != null) {
                Object o = di.o(aVar);
                if (o instanceof List) {
                    for (Object o2 : (List) o2) {
                        if (o2 instanceof Map) {
                            for (Entry entry : ((Map) o2).entrySet()) {
                                buildUpon.appendQueryParameter(entry.getKey().toString(), entry.getValue().toString());
                            }
                        } else {
                            bh.t("ArbitraryPixel: additional params contains non-map: not sending partial hit: " + buildUpon.build().toString());
                            return;
                        }
                    }
                }
                bh.t("ArbitraryPixel: additional params not a list: not sending partial hit: " + buildUpon.build().toString());
                return;
            }
            String uri = buildUpon.build().toString();
            this.TJ.iz().bk(uri);
            bh.v("ArbitraryPixel: url = " + uri);
            if (j != null) {
                synchronized (i.class) {
                    TI.add(j);
                    cz.a(this.mContext, TH, j, "true");
                }
            }
        }
    }
}
