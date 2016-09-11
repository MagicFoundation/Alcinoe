package com.google.android.gms.internal;

import android.content.Context;
import android.os.SystemClock;
import android.text.TextUtils;
import com.google.android.gms.internal.ch.a;
import com.google.android.gms.location.GeofenceStatusCodes;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.List;
import java.util.Map;

public final class ci extends a {
    private static final Object op;
    private static ci oq;
    private final Context mContext;
    private final au or;

    /* renamed from: com.google.android.gms.internal.ci.1 */
    static class AnonymousClass1 implements Runnable {
        final /* synthetic */ Context os;
        final /* synthetic */ cd ot;
        final /* synthetic */ ck ou;
        final /* synthetic */ String ov;

        AnonymousClass1(Context context, cd cdVar, ck ckVar, String str) {
            this.os = context;
            this.ot = cdVar;
            this.ou = ckVar;
            this.ov = str;
        }

        public void run() {
            dd a = dd.a(this.os, new ab(), false, false, null, this.ot.kN);
            a.setWillNotDraw(true);
            this.ou.b(a);
            de bb = a.bb();
            bb.a("/invalidRequest", this.ou.oz);
            bb.a("/loadAdURL", this.ou.oA);
            bb.a("/log", aq.mb);
            da.s("Getting the ad request URL.");
            a.loadDataWithBaseURL("http://googleads.g.doubleclick.net", "<!DOCTYPE html><html><head><script src=\"http://googleads.g.doubleclick.net/mads/static/sdk/native/sdk-core-v40.js\"></script><script>AFMA_buildAdURL(" + this.ov + ");</script></head><body></body></html>", "text/html", "UTF-8", null);
        }
    }

    static {
        op = new Object();
    }

    private ci(Context context, au auVar) {
        this.mContext = context;
        this.or = auVar;
    }

    private static cf a(Context context, au auVar, cd cdVar) {
        da.s("Starting ad request from service.");
        auVar.init();
        cm cmVar = new cm(context);
        if (cmVar.oX == -1) {
            da.s("Device is offline.");
            return new cf(2);
        }
        ck ckVar = new ck(cdVar.applicationInfo.packageName);
        if (cdVar.oc.extras != null) {
            String string = cdVar.oc.extras.getString("_ad");
            if (string != null) {
                return cj.a(context, cdVar, string);
            }
        }
        String a = cj.a(cdVar, cmVar, auVar.a(250));
        if (a == null) {
            return new cf(0);
        }
        cz.pT.post(new AnonymousClass1(context, cdVar, ckVar, a));
        a = ckVar.aI();
        return TextUtils.isEmpty(a) ? new cf(ckVar.getErrorCode()) : a(context, cdVar.kN.pU, a);
    }

    public static cf a(Context context, String str, String str2) {
        HttpURLConnection httpURLConnection;
        try {
            int responseCode;
            cf cfVar;
            cl clVar = new cl();
            URL url = new URL(str2);
            long elapsedRealtime = SystemClock.elapsedRealtime();
            URL url2 = url;
            int i = 0;
            while (true) {
                httpURLConnection = (HttpURLConnection) url2.openConnection();
                cv.a(context, str, false, httpURLConnection);
                responseCode = httpURLConnection.getResponseCode();
                Map headerFields = httpURLConnection.getHeaderFields();
                if (responseCode < 200 || responseCode >= 300) {
                    a(url2.toString(), headerFields, null, responseCode);
                    if (responseCode < 300 || responseCode >= 400) {
                        break;
                    }
                    Object headerField = httpURLConnection.getHeaderField("Location");
                    if (TextUtils.isEmpty(headerField)) {
                        da.w("No location header to follow redirect.");
                        cfVar = new cf(0);
                        httpURLConnection.disconnect();
                        return cfVar;
                    }
                    url2 = new URL(headerField);
                    i++;
                    if (i > 5) {
                        da.w("Too many redirects.");
                        cfVar = new cf(0);
                        httpURLConnection.disconnect();
                        return cfVar;
                    }
                    clVar.d(headerFields);
                    httpURLConnection.disconnect();
                } else {
                    String url3 = url2.toString();
                    String a = cv.a(new InputStreamReader(httpURLConnection.getInputStream()));
                    a(url3, headerFields, a, responseCode);
                    clVar.a(url3, headerFields, a);
                    cfVar = clVar.f(elapsedRealtime);
                    httpURLConnection.disconnect();
                    return cfVar;
                }
            }
            da.w("Received error HTTP response code: " + responseCode);
            cfVar = new cf(0);
            httpURLConnection.disconnect();
            return cfVar;
        } catch (IOException e) {
            da.w("Error while connecting to ad server: " + e.getMessage());
            return new cf(2);
        } catch (Throwable th) {
            httpURLConnection.disconnect();
        }
    }

    public static ci a(Context context, au auVar) {
        ci ciVar;
        synchronized (op) {
            if (oq == null) {
                oq = new ci(context.getApplicationContext(), auVar);
            }
            ciVar = oq;
        }
        return ciVar;
    }

    private static void a(String str, Map<String, List<String>> map, String str2, int i) {
        if (da.n(2)) {
            da.v("Http Response: {\n  URL:\n    " + str + "\n  Headers:");
            if (map != null) {
                for (String str3 : map.keySet()) {
                    da.v("    " + str3 + ":");
                    for (String str32 : (List) map.get(str32)) {
                        da.v("      " + str32);
                    }
                }
            }
            da.v("  Body:");
            if (str2 != null) {
                for (int i2 = 0; i2 < Math.min(str2.length(), 100000); i2 += GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE) {
                    da.v(str2.substring(i2, Math.min(str2.length(), i2 + GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE)));
                }
            } else {
                da.v("    null");
            }
            da.v("  Response Code:\n    " + i + "\n}");
        }
    }

    public cf b(cd cdVar) {
        return a(this.mContext, this.or, cdVar);
    }
}
