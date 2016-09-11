package com.google.android.gms.tagmanager;

import android.content.Context;
import com.google.analytics.tracking.android.ModelFields;
import com.google.android.gms.analytics.Tracker;
import com.google.android.gms.internal.a;
import com.google.android.gms.internal.b;
import com.google.android.gms.internal.d;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

class dj extends dg {
    private static final String ID;
    private static final String XR;
    private static final String XS;
    private static final String XT;
    private static final String XU;
    private static final String XV;
    private static final String XW;
    private static Map<String, String> XX;
    private static Map<String, String> XY;
    private final DataLayer TN;
    private final Set<String> XZ;
    private final df Ya;

    static {
        ID = a.UNIVERSAL_ANALYTICS.toString();
        XR = b.ACCOUNT.toString();
        XS = b.ANALYTICS_PASS_THROUGH.toString();
        XT = b.ANALYTICS_FIELDS.toString();
        XU = b.TRACK_TRANSACTION.toString();
        XV = b.TRANSACTION_DATALAYER_MAP.toString();
        XW = b.TRANSACTION_ITEM_DATALAYER_MAP.toString();
    }

    public dj(Context context, DataLayer dataLayer) {
        this(context, dataLayer, new df(context));
    }

    dj(Context context, DataLayer dataLayer, df dfVar) {
        super(ID, new String[0]);
        this.TN = dataLayer;
        this.Ya = dfVar;
        this.XZ = new HashSet();
        this.XZ.add("");
        this.XZ.add("0");
        this.XZ.add("false");
    }

    private Map<String, String> E(Map<String, d.a> map) {
        d.a aVar = (d.a) map.get(XV);
        if (aVar != null) {
            return c(aVar);
        }
        if (XX == null) {
            Map hashMap = new HashMap();
            hashMap.put(ModelFields.TRANSACTION_ID, "&ti");
            hashMap.put(ModelFields.TRANSACTION_AFFILIATION, "&ta");
            hashMap.put(ModelFields.TRANSACTION_TAX, "&tt");
            hashMap.put(ModelFields.TRANSACTION_SHIPPING, "&ts");
            hashMap.put(ModelFields.TRANSACTION_TOTAL, "&tr");
            hashMap.put("transactionCurrency", "&cu");
            XX = hashMap;
        }
        return XX;
    }

    private Map<String, String> F(Map<String, d.a> map) {
        d.a aVar = (d.a) map.get(XW);
        if (aVar != null) {
            return c(aVar);
        }
        if (XY == null) {
            Map hashMap = new HashMap();
            hashMap.put("name", "&in");
            hashMap.put("sku", "&ic");
            hashMap.put("category", "&iv");
            hashMap.put("price", "&ip");
            hashMap.put("quantity", "&iq");
            hashMap.put("currency", "&cu");
            XY = hashMap;
        }
        return XY;
    }

    private void a(Tracker tracker, Map<String, d.a> map) {
        String bN = bN(ModelFields.TRANSACTION_ID);
        if (bN == null) {
            bh.t("Cannot find transactionId in data layer.");
            return;
        }
        List<Map> linkedList = new LinkedList();
        try {
            Map p = p((d.a) map.get(XT));
            p.put("&t", "transaction");
            for (Entry entry : E(map).entrySet()) {
                b(p, (String) entry.getValue(), bN((String) entry.getKey()));
            }
            linkedList.add(p);
            List<Map> kv = kv();
            if (kv != null) {
                for (Map map2 : kv) {
                    if (map2.get("name") == null) {
                        bh.t("Unable to send transaction item hit due to missing 'name' field.");
                        return;
                    }
                    Map p2 = p((d.a) map.get(XT));
                    p2.put("&t", ModelFields.ITEM);
                    p2.put("&ti", bN);
                    for (Entry entry2 : F(map).entrySet()) {
                        b(p2, (String) entry2.getValue(), (String) map2.get(entry2.getKey()));
                    }
                    linkedList.add(p2);
                }
            }
            for (Map map22 : linkedList) {
                tracker.send(map22);
            }
        } catch (Throwable e) {
            bh.c("Unable to send transaction", e);
        }
    }

    private void b(Map<String, String> map, String str, String str2) {
        if (str2 != null) {
            map.put(str, str2);
        }
    }

    private String bN(String str) {
        Object obj = this.TN.get(str);
        return obj == null ? null : obj.toString();
    }

    private Map<String, String> c(d.a aVar) {
        Object o = di.o(aVar);
        if (!(o instanceof Map)) {
            return null;
        }
        Map map = (Map) o;
        Map<String, String> linkedHashMap = new LinkedHashMap();
        for (Entry entry : map.entrySet()) {
            linkedHashMap.put(entry.getKey().toString(), entry.getValue().toString());
        }
        return linkedHashMap;
    }

    private boolean d(Map<String, d.a> map, String str) {
        d.a aVar = (d.a) map.get(str);
        return aVar == null ? false : di.n(aVar).booleanValue();
    }

    private List<Map<String, String>> kv() {
        Object obj = this.TN.get("transactionProducts");
        if (obj == null) {
            return null;
        }
        if (obj instanceof List) {
            for (Object obj2 : (List) obj) {
                if (!(obj2 instanceof Map)) {
                    throw new IllegalArgumentException("Each element of transactionProducts should be of type Map.");
                }
            }
            return (List) obj;
        }
        throw new IllegalArgumentException("transactionProducts should be of type List.");
    }

    private Map<String, String> p(d.a aVar) {
        if (aVar == null) {
            return new HashMap();
        }
        Map<String, String> c = c(aVar);
        if (c == null) {
            return new HashMap();
        }
        String str = (String) c.get("&aip");
        if (str != null && this.XZ.contains(str.toLowerCase())) {
            c.remove("&aip");
        }
        return c;
    }

    public void w(Map<String, d.a> map) {
        Tracker bF = this.Ya.bF("_GTM_DEFAULT_TRACKER_");
        if (d(map, XS)) {
            bF.send(p((d.a) map.get(XT)));
        } else if (d(map, XU)) {
            a(bF, map);
        } else {
            bh.w("Ignoring unknown tag.");
        }
    }
}
