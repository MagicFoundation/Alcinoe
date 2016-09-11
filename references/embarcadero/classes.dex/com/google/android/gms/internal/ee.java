package com.google.android.gms.internal;

import android.os.Parcel;
import android.view.View;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public final class ee {
    private final a Bo;
    private final View zp;

    public static final class a implements SafeParcelable {
        public static final eq CREATOR;
        private final List<String> Bp;
        private final String vi;
        private final int wj;
        private final int zo;
        private final String zq;

        static {
            CREATOR = new eq();
        }

        a(int i, String str, List<String> list, int i2, String str2) {
            this.Bp = new ArrayList();
            this.wj = i;
            this.vi = str;
            this.Bp.addAll(list);
            this.zo = i2;
            this.zq = str2;
        }

        public a(String str, Collection<String> collection, int i, String str2) {
            this(3, str, new ArrayList(collection), i, str2);
        }

        public String dR() {
            return this.vi != null ? this.vi : "<<default account>>";
        }

        public int dS() {
            return this.zo;
        }

        public List<String> dT() {
            return new ArrayList(this.Bp);
        }

        public String dV() {
            return this.zq;
        }

        public int describeContents() {
            return 0;
        }

        public String getAccountName() {
            return this.vi;
        }

        public int getVersionCode() {
            return this.wj;
        }

        public void writeToParcel(Parcel out, int flags) {
            eq.a(this, out, flags);
        }
    }

    public ee(String str, Collection<String> collection, int i, View view, String str2) {
        this.Bo = new a(str, collection, i, str2);
        this.zp = view;
    }

    public String dR() {
        return this.Bo.dR();
    }

    public int dS() {
        return this.Bo.dS();
    }

    public List<String> dT() {
        return this.Bo.dT();
    }

    public String[] dU() {
        return (String[]) this.Bo.dT().toArray(new String[0]);
    }

    public String dV() {
        return this.Bo.dV();
    }

    public View dW() {
        return this.zp;
    }

    public String getAccountName() {
        return this.Bo.getAccountName();
    }
}
