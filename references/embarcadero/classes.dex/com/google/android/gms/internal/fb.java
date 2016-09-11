package com.google.android.gms.internal;

import android.os.Parcel;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.games.GamesStatusCodes;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public abstract class fb {

    public interface b<I, O> {
        int el();

        int em();

        I g(O o);
    }

    public static class a<I, O> implements SafeParcelable {
        public static final fc CREATOR;
        protected final Class<? extends fb> CA;
        protected final String CB;
        private fe CC;
        private b<I, O> CD;
        protected final int Cu;
        protected final boolean Cv;
        protected final int Cw;
        protected final boolean Cx;
        protected final String Cy;
        protected final int Cz;
        private final int wj;

        static {
            CREATOR = new fc();
        }

        a(int i, int i2, boolean z, int i3, boolean z2, String str, int i4, String str2, ew ewVar) {
            this.wj = i;
            this.Cu = i2;
            this.Cv = z;
            this.Cw = i3;
            this.Cx = z2;
            this.Cy = str;
            this.Cz = i4;
            if (str2 == null) {
                this.CA = null;
                this.CB = null;
            } else {
                this.CA = fh.class;
                this.CB = str2;
            }
            if (ewVar == null) {
                this.CD = null;
            } else {
                this.CD = ewVar.ej();
            }
        }

        protected a(int i, boolean z, int i2, boolean z2, String str, int i3, Class<? extends fb> cls, b<I, O> bVar) {
            this.wj = 1;
            this.Cu = i;
            this.Cv = z;
            this.Cw = i2;
            this.Cx = z2;
            this.Cy = str;
            this.Cz = i3;
            this.CA = cls;
            if (cls == null) {
                this.CB = null;
            } else {
                this.CB = cls.getCanonicalName();
            }
            this.CD = bVar;
        }

        public static a a(String str, int i, b<?, ?> bVar, boolean z) {
            return new a(bVar.el(), z, bVar.em(), false, str, i, null, bVar);
        }

        public static <T extends fb> a<T, T> a(String str, int i, Class<T> cls) {
            return new a(11, false, 11, false, str, i, cls, null);
        }

        public static <T extends fb> a<ArrayList<T>, ArrayList<T>> b(String str, int i, Class<T> cls) {
            return new a(11, true, 11, true, str, i, cls, null);
        }

        public static a<Integer, Integer> g(String str, int i) {
            return new a(0, false, 0, false, str, i, null, null);
        }

        public static a<Double, Double> h(String str, int i) {
            return new a(4, false, 4, false, str, i, null, null);
        }

        public static a<Boolean, Boolean> i(String str, int i) {
            return new a(6, false, 6, false, str, i, null, null);
        }

        public static a<String, String> j(String str, int i) {
            return new a(7, false, 7, false, str, i, null, null);
        }

        public static a<ArrayList<String>, ArrayList<String>> k(String str, int i) {
            return new a(7, true, 7, true, str, i, null, null);
        }

        public void a(fe feVar) {
            this.CC = feVar;
        }

        public int describeContents() {
            fc fcVar = CREATOR;
            return 0;
        }

        public int el() {
            return this.Cu;
        }

        public int em() {
            return this.Cw;
        }

        public a<I, O> eq() {
            return new a(this.wj, this.Cu, this.Cv, this.Cw, this.Cx, this.Cy, this.Cz, this.CB, ey());
        }

        public boolean er() {
            return this.Cv;
        }

        public boolean es() {
            return this.Cx;
        }

        public String et() {
            return this.Cy;
        }

        public int eu() {
            return this.Cz;
        }

        public Class<? extends fb> ev() {
            return this.CA;
        }

        String ew() {
            return this.CB == null ? null : this.CB;
        }

        public boolean ex() {
            return this.CD != null;
        }

        ew ey() {
            return this.CD == null ? null : ew.a(this.CD);
        }

        public HashMap<String, a<?, ?>> ez() {
            er.f(this.CB);
            er.f(this.CC);
            return this.CC.ao(this.CB);
        }

        public I g(O o) {
            return this.CD.g(o);
        }

        public int getVersionCode() {
            return this.wj;
        }

        public String toString() {
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.append("Field\n");
            stringBuilder.append("            versionCode=").append(this.wj).append('\n');
            stringBuilder.append("                 typeIn=").append(this.Cu).append('\n');
            stringBuilder.append("            typeInArray=").append(this.Cv).append('\n');
            stringBuilder.append("                typeOut=").append(this.Cw).append('\n');
            stringBuilder.append("           typeOutArray=").append(this.Cx).append('\n');
            stringBuilder.append("        outputFieldName=").append(this.Cy).append('\n');
            stringBuilder.append("      safeParcelFieldId=").append(this.Cz).append('\n');
            stringBuilder.append("       concreteTypeName=").append(ew()).append('\n');
            if (ev() != null) {
                stringBuilder.append("     concreteType.class=").append(ev().getCanonicalName()).append('\n');
            }
            stringBuilder.append("          converterName=").append(this.CD == null ? "null" : this.CD.getClass().getCanonicalName()).append('\n');
            return stringBuilder.toString();
        }

        public void writeToParcel(Parcel out, int flags) {
            fc fcVar = CREATOR;
            fc.a(this, out, flags);
        }
    }

    private void a(StringBuilder stringBuilder, a aVar, Object obj) {
        if (aVar.el() == 11) {
            stringBuilder.append(((fb) aVar.ev().cast(obj)).toString());
        } else if (aVar.el() == 7) {
            stringBuilder.append("\"");
            stringBuilder.append(fp.ap((String) obj));
            stringBuilder.append("\"");
        } else {
            stringBuilder.append(obj);
        }
    }

    private void a(StringBuilder stringBuilder, a aVar, ArrayList<Object> arrayList) {
        stringBuilder.append("[");
        int size = arrayList.size();
        for (int i = 0; i < size; i++) {
            if (i > 0) {
                stringBuilder.append(",");
            }
            Object obj = arrayList.get(i);
            if (obj != null) {
                a(stringBuilder, aVar, obj);
            }
        }
        stringBuilder.append("]");
    }

    protected <O, I> I a(a<I, O> aVar, Object obj) {
        return aVar.CD != null ? aVar.g(obj) : obj;
    }

    protected boolean a(a aVar) {
        return aVar.em() == 11 ? aVar.es() ? an(aVar.et()) : am(aVar.et()) : al(aVar.et());
    }

    protected abstract Object ak(String str);

    protected abstract boolean al(String str);

    protected boolean am(String str) {
        throw new UnsupportedOperationException("Concrete types not supported");
    }

    protected boolean an(String str) {
        throw new UnsupportedOperationException("Concrete type arrays not supported");
    }

    protected Object b(a aVar) {
        boolean z = true;
        String et = aVar.et();
        if (aVar.ev() == null) {
            return ak(aVar.et());
        }
        if (ak(aVar.et()) != null) {
            z = false;
        }
        er.a(z, "Concrete field shouldn't be value object: " + aVar.et());
        Map ep = aVar.es() ? ep() : eo();
        if (ep != null) {
            return ep.get(et);
        }
        try {
            return getClass().getMethod("get" + Character.toUpperCase(et.charAt(0)) + et.substring(1), new Class[0]).invoke(this, new Object[0]);
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }

    public abstract HashMap<String, a<?, ?>> en();

    public HashMap<String, Object> eo() {
        return null;
    }

    public HashMap<String, Object> ep() {
        return null;
    }

    public String toString() {
        HashMap en = en();
        StringBuilder stringBuilder = new StringBuilder(100);
        for (String str : en.keySet()) {
            a aVar = (a) en.get(str);
            if (a(aVar)) {
                Object a = a(aVar, b(aVar));
                if (stringBuilder.length() == 0) {
                    stringBuilder.append("{");
                } else {
                    stringBuilder.append(",");
                }
                stringBuilder.append("\"").append(str).append("\":");
                if (a != null) {
                    switch (aVar.em()) {
                        case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                            stringBuilder.append("\"").append(fk.d((byte[]) a)).append("\"");
                            break;
                        case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                            stringBuilder.append("\"").append(fk.e((byte[]) a)).append("\"");
                            break;
                        case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                            fq.a(stringBuilder, (HashMap) a);
                            break;
                        default:
                            if (!aVar.er()) {
                                a(stringBuilder, aVar, a);
                                break;
                            }
                            a(stringBuilder, aVar, (ArrayList) a);
                            break;
                    }
                }
                stringBuilder.append("null");
            }
        }
        if (stringBuilder.length() > 0) {
            stringBuilder.append("}");
        } else {
            stringBuilder.append("{}");
        }
        return stringBuilder.toString();
    }
}
