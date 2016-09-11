package com.google.android.gms.internal;

import com.google.android.gms.common.data.DataHolder;
import java.util.HashMap;
import java.util.Set;

public final class hb {
    private static final String[] IH;
    private final HashMap<String, Integer> II;
    private final int yJ;

    public static final class a {
        private HashMap<String, Integer> II;
        private int yJ;

        public a() {
            this.II = new HashMap();
            this.yJ = 0;
        }

        public a aZ(int i) {
            this.yJ = i;
            return this;
        }

        public hb fV() {
            return new hb(this.II, null);
        }

        public a p(String str, int i) {
            if (gt.isValid(i)) {
                this.II.put(str, Integer.valueOf(i));
            }
            return this;
        }
    }

    static {
        IH = new String[]{"requestId", "outcome"};
    }

    private hb(int i, HashMap<String, Integer> hashMap) {
        this.yJ = i;
        this.II = hashMap;
    }

    public static hb H(DataHolder dataHolder) {
        a aVar = new a();
        aVar.aZ(dataHolder.getStatusCode());
        int count = dataHolder.getCount();
        for (int i = 0; i < count; i++) {
            int I = dataHolder.I(i);
            aVar.p(dataHolder.getString("requestId", i, I), dataHolder.getInteger("outcome", i, I));
        }
        return aVar.fV();
    }

    public Set<String> getRequestIds() {
        return this.II.keySet();
    }

    public int getRequestOutcome(String requestId) {
        er.b(this.II.containsKey(requestId), "Request " + requestId + " was not part of the update operation!");
        return ((Integer) this.II.get(requestId)).intValue();
    }
}
