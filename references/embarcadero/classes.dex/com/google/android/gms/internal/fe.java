package com.google.android.gms.internal;

import android.os.Parcel;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import java.util.ArrayList;
import java.util.HashMap;

public class fe implements SafeParcelable {
    public static final ff CREATOR;
    private final HashMap<String, HashMap<String, com.google.android.gms.internal.fb.a<?, ?>>> CE;
    private final ArrayList<a> CF;
    private final String CG;
    private final int wj;

    public static class a implements SafeParcelable {
        public static final fg CREATOR;
        final ArrayList<b> CH;
        final String className;
        final int versionCode;

        static {
            CREATOR = new fg();
        }

        a(int i, String str, ArrayList<b> arrayList) {
            this.versionCode = i;
            this.className = str;
            this.CH = arrayList;
        }

        a(String str, HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> hashMap) {
            this.versionCode = 1;
            this.className = str;
            this.CH = b(hashMap);
        }

        private static ArrayList<b> b(HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> hashMap) {
            if (hashMap == null) {
                return null;
            }
            ArrayList<b> arrayList = new ArrayList();
            for (String str : hashMap.keySet()) {
                arrayList.add(new b(str, (com.google.android.gms.internal.fb.a) hashMap.get(str)));
            }
            return arrayList;
        }

        public int describeContents() {
            fg fgVar = CREATOR;
            return 0;
        }

        HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> eE() {
            HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> hashMap = new HashMap();
            int size = this.CH.size();
            for (int i = 0; i < size; i++) {
                b bVar = (b) this.CH.get(i);
                hashMap.put(bVar.eX, bVar.CI);
            }
            return hashMap;
        }

        public void writeToParcel(Parcel out, int flags) {
            fg fgVar = CREATOR;
            fg.a(this, out, flags);
        }
    }

    public static class b implements SafeParcelable {
        public static final fd CREATOR;
        final com.google.android.gms.internal.fb.a<?, ?> CI;
        final String eX;
        final int versionCode;

        static {
            CREATOR = new fd();
        }

        b(int i, String str, com.google.android.gms.internal.fb.a<?, ?> aVar) {
            this.versionCode = i;
            this.eX = str;
            this.CI = aVar;
        }

        b(String str, com.google.android.gms.internal.fb.a<?, ?> aVar) {
            this.versionCode = 1;
            this.eX = str;
            this.CI = aVar;
        }

        public int describeContents() {
            fd fdVar = CREATOR;
            return 0;
        }

        public void writeToParcel(Parcel out, int flags) {
            fd fdVar = CREATOR;
            fd.a(this, out, flags);
        }
    }

    static {
        CREATOR = new ff();
    }

    fe(int i, ArrayList<a> arrayList, String str) {
        this.wj = i;
        this.CF = null;
        this.CE = b((ArrayList) arrayList);
        this.CG = (String) er.f(str);
        eA();
    }

    public fe(Class<? extends fb> cls) {
        this.wj = 1;
        this.CF = null;
        this.CE = new HashMap();
        this.CG = cls.getCanonicalName();
    }

    private static HashMap<String, HashMap<String, com.google.android.gms.internal.fb.a<?, ?>>> b(ArrayList<a> arrayList) {
        HashMap<String, HashMap<String, com.google.android.gms.internal.fb.a<?, ?>>> hashMap = new HashMap();
        int size = arrayList.size();
        for (int i = 0; i < size; i++) {
            a aVar = (a) arrayList.get(i);
            hashMap.put(aVar.className, aVar.eE());
        }
        return hashMap;
    }

    public void a(Class<? extends fb> cls, HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> hashMap) {
        this.CE.put(cls.getCanonicalName(), hashMap);
    }

    public HashMap<String, com.google.android.gms.internal.fb.a<?, ?>> ao(String str) {
        return (HashMap) this.CE.get(str);
    }

    public boolean b(Class<? extends fb> cls) {
        return this.CE.containsKey(cls.getCanonicalName());
    }

    public int describeContents() {
        ff ffVar = CREATOR;
        return 0;
    }

    public void eA() {
        for (String str : this.CE.keySet()) {
            HashMap hashMap = (HashMap) this.CE.get(str);
            for (String str2 : hashMap.keySet()) {
                ((com.google.android.gms.internal.fb.a) hashMap.get(str2)).a(this);
            }
        }
    }

    public void eB() {
        for (String str : this.CE.keySet()) {
            HashMap hashMap = (HashMap) this.CE.get(str);
            HashMap hashMap2 = new HashMap();
            for (String str2 : hashMap.keySet()) {
                hashMap2.put(str2, ((com.google.android.gms.internal.fb.a) hashMap.get(str2)).eq());
            }
            this.CE.put(str, hashMap2);
        }
    }

    ArrayList<a> eC() {
        ArrayList<a> arrayList = new ArrayList();
        for (String str : this.CE.keySet()) {
            arrayList.add(new a(str, (HashMap) this.CE.get(str)));
        }
        return arrayList;
    }

    public String eD() {
        return this.CG;
    }

    int getVersionCode() {
        return this.wj;
    }

    public String toString() {
        StringBuilder stringBuilder = new StringBuilder();
        for (String str : this.CE.keySet()) {
            stringBuilder.append(str).append(":\n");
            HashMap hashMap = (HashMap) this.CE.get(str);
            for (String str2 : hashMap.keySet()) {
                stringBuilder.append("  ").append(str2).append(": ");
                stringBuilder.append(hashMap.get(str2));
            }
        }
        return stringBuilder.toString();
    }

    public void writeToParcel(Parcel out, int flags) {
        ff ffVar = CREATOR;
        ff.a(this, out, flags);
    }
}
