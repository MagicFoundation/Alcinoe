package com.google.android.gms.internal;

import android.os.Bundle;
import android.os.SystemClock;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;

public class co {
    private final Object mg;
    private boolean oI;
    private final LinkedList<a> pj;
    private final String pk;
    private final String pl;
    private long pm;
    private long pn;
    private long po;
    private long pp;
    private long pq;
    private long pr;

    private static final class a {
        private long ps;
        private long pt;

        public a() {
            this.ps = -1;
            this.pt = -1;
        }

        public long aM() {
            return this.pt;
        }

        public void aN() {
            this.pt = SystemClock.elapsedRealtime();
        }

        public void aO() {
            this.ps = SystemClock.elapsedRealtime();
        }

        public Bundle toBundle() {
            Bundle bundle = new Bundle();
            bundle.putLong("topen", this.ps);
            bundle.putLong("tclose", this.pt);
            return bundle;
        }
    }

    public co(String str, String str2) {
        this.mg = new Object();
        this.pm = -1;
        this.pn = -1;
        this.oI = false;
        this.po = -1;
        this.pp = 0;
        this.pq = -1;
        this.pr = -1;
        this.pk = str;
        this.pl = str2;
        this.pj = new LinkedList();
    }

    public void aJ() {
        synchronized (this.mg) {
            if (!(this.pr == -1 || this.pn == -1)) {
                this.pn = SystemClock.elapsedRealtime();
                cp.aQ().aJ();
                cp.a(this);
            }
        }
    }

    public void aK() {
        synchronized (this.mg) {
            if (this.pr != -1) {
                a aVar = new a();
                aVar.aO();
                this.pj.add(aVar);
                this.pp++;
                cp.aQ().aK();
                cp.a(this);
            }
        }
    }

    public void aL() {
        synchronized (this.mg) {
            if (!(this.pr == -1 || this.pj.isEmpty())) {
                a aVar = (a) this.pj.getLast();
                if (aVar.aM() == -1) {
                    aVar.aN();
                    cp.a(this);
                }
            }
        }
    }

    public void f(z zVar) {
        synchronized (this.mg) {
            this.pq = SystemClock.elapsedRealtime();
            cp.aQ().b(zVar, this.pq);
        }
    }

    public void g(long j) {
        synchronized (this.mg) {
            this.pr = j;
            if (this.pr != -1) {
                cp.a(this);
            }
        }
    }

    public void h(long j) {
        synchronized (this.mg) {
            if (this.pr != -1) {
                this.pm = j;
                cp.a(this);
            }
        }
    }

    public void k(boolean z) {
        synchronized (this.mg) {
            if (this.pr != -1) {
                this.po = SystemClock.elapsedRealtime();
                if (!z) {
                    this.pn = this.po;
                    cp.a(this);
                }
            }
        }
    }

    public void l(boolean z) {
        synchronized (this.mg) {
            if (this.pr != -1) {
                this.oI = z;
                cp.a(this);
            }
        }
    }

    public Bundle toBundle() {
        Bundle bundle;
        synchronized (this.mg) {
            bundle = new Bundle();
            bundle.putString("seqnum", this.pk);
            bundle.putString("slotid", this.pl);
            bundle.putBoolean("ismediation", this.oI);
            bundle.putLong("treq", this.pq);
            bundle.putLong("tresponse", this.pr);
            bundle.putLong("timp", this.pn);
            bundle.putLong("tload", this.po);
            bundle.putLong("pcc", this.pp);
            bundle.putLong("tfetch", this.pm);
            ArrayList arrayList = new ArrayList();
            Iterator it = this.pj.iterator();
            while (it.hasNext()) {
                arrayList.add(((a) it.next()).toBundle());
            }
            bundle.putParcelableArrayList("tclick", arrayList);
        }
        return bundle;
    }
}
