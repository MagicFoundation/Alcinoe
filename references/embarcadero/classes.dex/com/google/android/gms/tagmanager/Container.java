package com.google.android.gms.tagmanager;

import android.content.Context;
import com.google.android.gms.internal.c.f;
import com.google.android.gms.internal.c.i;
import com.google.android.gms.internal.c.j;
import com.google.android.gms.tagmanager.cr.c;
import com.google.android.gms.tagmanager.cr.g;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Container {
    private final String TM;
    private final DataLayer TN;
    private ct TO;
    private Map<String, FunctionCallMacroCallback> TP;
    private Map<String, FunctionCallTagCallback> TQ;
    private volatile long TR;
    private volatile String TS;
    private final Context mContext;

    public interface FunctionCallMacroCallback {
        Object getValue(String str, Map<String, Object> map);
    }

    public interface FunctionCallTagCallback {
        void execute(String str, Map<String, Object> map);
    }

    private class a implements com.google.android.gms.tagmanager.s.a {
        final /* synthetic */ Container TT;

        private a(Container container) {
            this.TT = container;
        }

        public Object b(String str, Map<String, Object> map) {
            FunctionCallMacroCallback aY = this.TT.aY(str);
            return aY == null ? null : aY.getValue(str, map);
        }
    }

    private class b implements com.google.android.gms.tagmanager.s.a {
        final /* synthetic */ Container TT;

        private b(Container container) {
            this.TT = container;
        }

        public Object b(String str, Map<String, Object> map) {
            FunctionCallTagCallback aZ = this.TT.aZ(str);
            if (aZ != null) {
                aZ.execute(str, map);
            }
            return di.kt();
        }
    }

    Container(Context context, DataLayer dataLayer, String containerId, long lastRefreshTime, j resource) {
        this.TP = new HashMap();
        this.TQ = new HashMap();
        this.TS = "";
        this.mContext = context;
        this.TN = dataLayer;
        this.TM = containerId;
        this.TR = lastRefreshTime;
        a(resource.fV);
        if (resource.fU != null) {
            a(resource.fU);
        }
    }

    Container(Context context, DataLayer dataLayer, String containerId, long lastRefreshTime, c resource) {
        this.TP = new HashMap();
        this.TQ = new HashMap();
        this.TS = "";
        this.mContext = context;
        this.TN = dataLayer;
        this.TM = containerId;
        this.TR = lastRefreshTime;
        a(resource);
    }

    private void a(f fVar) {
        if (fVar == null) {
            throw new NullPointerException();
        }
        try {
            a(cr.b(fVar));
        } catch (g e) {
            bh.t("Not loading resource: " + fVar + " because it is invalid: " + e.toString());
        }
    }

    private void a(c cVar) {
        this.TS = cVar.getVersion();
        c cVar2 = cVar;
        a(new ct(this.mContext, cVar2, this.TN, new a(), new b(), bb(this.TS)));
    }

    private synchronized void a(ct ctVar) {
        this.TO = ctVar;
    }

    private void a(i[] iVarArr) {
        List arrayList = new ArrayList();
        for (Object add : iVarArr) {
            arrayList.add(add);
        }
        iE().f(arrayList);
    }

    private synchronized ct iE() {
        return this.TO;
    }

    FunctionCallMacroCallback aY(String str) {
        FunctionCallMacroCallback functionCallMacroCallback;
        synchronized (this.TP) {
            functionCallMacroCallback = (FunctionCallMacroCallback) this.TP.get(str);
        }
        return functionCallMacroCallback;
    }

    FunctionCallTagCallback aZ(String str) {
        FunctionCallTagCallback functionCallTagCallback;
        synchronized (this.TQ) {
            functionCallTagCallback = (FunctionCallTagCallback) this.TQ.get(str);
        }
        return functionCallTagCallback;
    }

    void ba(String str) {
        iE().ba(str);
    }

    ag bb(String str) {
        if (ce.ju().jv().equals(a.CONTAINER_DEBUG)) {
        }
        return new bq();
    }

    public boolean getBoolean(String key) {
        ct iE = iE();
        if (iE == null) {
            bh.t("getBoolean called for closed container.");
            return di.kr().booleanValue();
        }
        try {
            return di.n((com.google.android.gms.internal.d.a) iE.bC(key).getObject()).booleanValue();
        } catch (Exception e) {
            bh.t("Calling getBoolean() threw an exception: " + e.getMessage() + " Returning default value.");
            return di.kr().booleanValue();
        }
    }

    public String getContainerId() {
        return this.TM;
    }

    public double getDouble(String key) {
        ct iE = iE();
        if (iE == null) {
            bh.t("getDouble called for closed container.");
            return di.kq().doubleValue();
        }
        try {
            return di.m((com.google.android.gms.internal.d.a) iE.bC(key).getObject()).doubleValue();
        } catch (Exception e) {
            bh.t("Calling getDouble() threw an exception: " + e.getMessage() + " Returning default value.");
            return di.kq().doubleValue();
        }
    }

    public long getLastRefreshTime() {
        return this.TR;
    }

    public long getLong(String key) {
        ct iE = iE();
        if (iE == null) {
            bh.t("getLong called for closed container.");
            return di.kp().longValue();
        }
        try {
            return di.l((com.google.android.gms.internal.d.a) iE.bC(key).getObject()).longValue();
        } catch (Exception e) {
            bh.t("Calling getLong() threw an exception: " + e.getMessage() + " Returning default value.");
            return di.kp().longValue();
        }
    }

    public String getString(String key) {
        ct iE = iE();
        if (iE == null) {
            bh.t("getString called for closed container.");
            return di.kt();
        }
        try {
            return di.j((com.google.android.gms.internal.d.a) iE.bC(key).getObject());
        } catch (Exception e) {
            bh.t("Calling getString() threw an exception: " + e.getMessage() + " Returning default value.");
            return di.kt();
        }
    }

    String iD() {
        return this.TS;
    }

    public boolean isDefault() {
        return getLastRefreshTime() == 0;
    }

    public void registerFunctionCallMacroCallback(String customMacroName, FunctionCallMacroCallback customMacroCallback) {
        if (customMacroCallback == null) {
            throw new NullPointerException("Macro handler must be non-null");
        }
        synchronized (this.TP) {
            this.TP.put(customMacroName, customMacroCallback);
        }
    }

    public void registerFunctionCallTagCallback(String customTagName, FunctionCallTagCallback customTagCallback) {
        if (customTagCallback == null) {
            throw new NullPointerException("Tag callback must be non-null");
        }
        synchronized (this.TQ) {
            this.TQ.put(customTagName, customTagCallback);
        }
    }

    void release() {
        this.TO = null;
    }

    public void unregisterFunctionCallMacroCallback(String customMacroName) {
        synchronized (this.TP) {
            this.TP.remove(customMacroName);
        }
    }

    public void unregisterFunctionCallTagCallback(String customTagName) {
        synchronized (this.TQ) {
            this.TQ.remove(customTagName);
        }
    }
}
