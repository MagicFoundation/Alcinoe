package com.google.android.gms.internal;

import android.content.Context;
import android.location.Location;
import com.google.android.gms.ads.mediation.NetworkExtras;
import com.google.android.gms.ads.search.SearchAdRequest;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public final class aj {
    public static final String DEVICE_ID_EMULATOR;
    private final Date d;
    private final Set<String> f;
    private final int lA;
    private final Set<String> lB;
    private final String lt;
    private final int lu;
    private final Location lv;
    private final boolean lw;
    private final Map<Class<? extends NetworkExtras>, NetworkExtras> lx;
    private final String ly;
    private final SearchAdRequest lz;

    public static final class a {
        private Date d;
        private int lA;
        private final HashSet<String> lC;
        private final HashMap<Class<? extends NetworkExtras>, NetworkExtras> lD;
        private final HashSet<String> lE;
        private String lt;
        private int lu;
        private Location lv;
        private boolean lw;
        private String ly;

        public a() {
            this.lC = new HashSet();
            this.lD = new HashMap();
            this.lE = new HashSet();
            this.lu = -1;
            this.lw = false;
            this.lA = -1;
        }

        public void a(Location location) {
            this.lv = location;
        }

        public void a(NetworkExtras networkExtras) {
            this.lD.put(networkExtras.getClass(), networkExtras);
        }

        public void a(Date date) {
            this.d = date;
        }

        public void d(int i) {
            this.lu = i;
        }

        public void d(boolean z) {
            this.lw = z;
        }

        public void e(boolean z) {
            this.lA = z ? 1 : 0;
        }

        public void g(String str) {
            this.lC.add(str);
        }

        public void h(String str) {
            this.lE.add(str);
        }

        public void i(String str) {
            this.lt = str;
        }

        public void j(String str) {
            this.ly = str;
        }
    }

    static {
        DEVICE_ID_EMULATOR = cz.r("emulator");
    }

    public aj(a aVar) {
        this(aVar, null);
    }

    public aj(a aVar, SearchAdRequest searchAdRequest) {
        this.d = aVar.d;
        this.lt = aVar.lt;
        this.lu = aVar.lu;
        this.f = Collections.unmodifiableSet(aVar.lC);
        this.lv = aVar.lv;
        this.lw = aVar.lw;
        this.lx = Collections.unmodifiableMap(aVar.lD);
        this.ly = aVar.ly;
        this.lz = searchAdRequest;
        this.lA = aVar.lA;
        this.lB = Collections.unmodifiableSet(aVar.lE);
    }

    public SearchAdRequest aj() {
        return this.lz;
    }

    public Map<Class<? extends NetworkExtras>, NetworkExtras> ak() {
        return this.lx;
    }

    public int al() {
        return this.lA;
    }

    public Date getBirthday() {
        return this.d;
    }

    public String getContentUrl() {
        return this.lt;
    }

    public int getGender() {
        return this.lu;
    }

    public Set<String> getKeywords() {
        return this.f;
    }

    public Location getLocation() {
        return this.lv;
    }

    public boolean getManualImpressionsEnabled() {
        return this.lw;
    }

    public <T extends NetworkExtras> T getNetworkExtras(Class<T> networkExtrasClass) {
        return (NetworkExtras) this.lx.get(networkExtrasClass);
    }

    public String getPublisherProvidedId() {
        return this.ly;
    }

    public boolean isTestDevice(Context context) {
        return this.lB.contains(cz.l(context));
    }
}
