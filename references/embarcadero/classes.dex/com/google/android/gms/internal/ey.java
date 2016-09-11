package com.google.android.gms.internal;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.internal.fb.b;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

public final class ey implements SafeParcelable, b<String, Integer> {
    public static final ez CREATOR;
    private final HashMap<String, Integer> Cp;
    private final HashMap<Integer, String> Cq;
    private final ArrayList<a> Cr;
    private final int wj;

    public static final class a implements SafeParcelable {
        public static final fa CREATOR;
        final String Cs;
        final int Ct;
        final int versionCode;

        static {
            CREATOR = new fa();
        }

        a(int i, String str, int i2) {
            this.versionCode = i;
            this.Cs = str;
            this.Ct = i2;
        }

        a(String str, int i) {
            this.versionCode = 1;
            this.Cs = str;
            this.Ct = i;
        }

        public int describeContents() {
            fa faVar = CREATOR;
            return 0;
        }

        public void writeToParcel(Parcel out, int flags) {
            fa faVar = CREATOR;
            fa.a(this, out, flags);
        }
    }

    static {
        CREATOR = new ez();
    }

    public ey() {
        this.wj = 1;
        this.Cp = new HashMap();
        this.Cq = new HashMap();
        this.Cr = null;
    }

    ey(int i, ArrayList<a> arrayList) {
        this.wj = i;
        this.Cp = new HashMap();
        this.Cq = new HashMap();
        this.Cr = null;
        a((ArrayList) arrayList);
    }

    private void a(ArrayList<a> arrayList) {
        Iterator it = arrayList.iterator();
        while (it.hasNext()) {
            a aVar = (a) it.next();
            f(aVar.Cs, aVar.Ct);
        }
    }

    public String a(Integer num) {
        String str = (String) this.Cq.get(num);
        return (str == null && this.Cp.containsKey("gms_unknown")) ? "gms_unknown" : str;
    }

    public int describeContents() {
        ez ezVar = CREATOR;
        return 0;
    }

    ArrayList<a> ek() {
        ArrayList<a> arrayList = new ArrayList();
        for (String str : this.Cp.keySet()) {
            arrayList.add(new a(str, ((Integer) this.Cp.get(str)).intValue()));
        }
        return arrayList;
    }

    public int el() {
        return 7;
    }

    public int em() {
        return 0;
    }

    public ey f(String str, int i) {
        this.Cp.put(str, Integer.valueOf(i));
        this.Cq.put(Integer.valueOf(i), str);
        return this;
    }

    public /* synthetic */ Object g(Object obj) {
        return a((Integer) obj);
    }

    int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel out, int flags) {
        ez ezVar = CREATOR;
        ez.a(this, out, flags);
    }
}
