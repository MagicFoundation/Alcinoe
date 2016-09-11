package com.google.android.gms.internal;

import com.google.android.gms.common.api.CommonStatusCodes;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;

public class kb<M extends ka<M>, T> {
    protected final Class<T> aaf;
    protected final boolean aag;
    protected final int tag;
    protected final int type;

    private kb(int i, Class<T> cls, int i2, boolean z) {
        this.type = i;
        this.aaf = cls;
        this.tag = i2;
        this.aag = z;
    }

    public static <M extends ka<M>, T extends ke> kb<M, T> a(int i, Class<T> cls, int i2) {
        return new kb(i, cls, i2, false);
    }

    protected void a(kg kgVar, List<Object> list) {
        list.add(o(jy.n(kgVar.aai)));
    }

    protected boolean cI(int i) {
        return i == this.tag;
    }

    final T g(List<kg> list) {
        int i = 0;
        if (list == null) {
            return null;
        }
        kg kgVar;
        if (this.aag) {
            int i2;
            List arrayList = new ArrayList();
            for (i2 = 0; i2 < list.size(); i2++) {
                kgVar = (kg) list.get(i2);
                if (cI(kgVar.tag) && kgVar.aai.length != 0) {
                    a(kgVar, arrayList);
                }
            }
            i2 = arrayList.size();
            if (i2 == 0) {
                return null;
            }
            T cast = this.aaf.cast(Array.newInstance(this.aaf.getComponentType(), i2));
            while (i < i2) {
                Array.set(cast, i, arrayList.get(i));
                i++;
            }
            return cast;
        }
        i = list.size() - 1;
        kg kgVar2 = null;
        while (kgVar2 == null && i >= 0) {
            kgVar = (kg) list.get(i);
            if (!cI(kgVar.tag) || kgVar.aai.length == 0) {
                kgVar = kgVar2;
            }
            i--;
            kgVar2 = kgVar;
        }
        return kgVar2 == null ? null : this.aaf.cast(o(jy.n(kgVar2.aai)));
    }

    protected Object o(jy jyVar) {
        Class componentType = this.aag ? this.aaf.getComponentType() : this.aaf;
        try {
            ke keVar;
            switch (this.type) {
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    keVar = (ke) componentType.newInstance();
                    jyVar.a(keVar, kh.cK(this.tag));
                    return keVar;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    keVar = (ke) componentType.newInstance();
                    jyVar.a(keVar);
                    return keVar;
                default:
                    throw new IllegalArgumentException("Unknown type " + this.type);
            }
        } catch (Throwable e) {
            throw new IllegalArgumentException("Error creating instance of class " + componentType, e);
        } catch (Throwable e2) {
            throw new IllegalArgumentException("Error creating instance of class " + componentType, e2);
        } catch (Throwable e22) {
            throw new IllegalArgumentException("Error reading extension field", e22);
        }
    }
}
