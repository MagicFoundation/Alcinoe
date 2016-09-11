package com.google.android.gms.internal;

import android.content.Context;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;

public final class cy extends ct {
    private final Context mContext;
    private final String pR;
    private final String pS;

    public cy(Context context, String str, String str2) {
        this.mContext = context;
        this.pR = str;
        this.pS = str2;
    }

    public void aB() {
        HttpURLConnection httpURLConnection;
        try {
            da.v("Pinging URL: " + this.pS);
            httpURLConnection = (HttpURLConnection) new URL(this.pS).openConnection();
            cv.a(this.mContext, this.pR, true, httpURLConnection);
            int responseCode = httpURLConnection.getResponseCode();
            if (responseCode < 200 || responseCode >= 300) {
                da.w("Received non-success response code " + responseCode + " from pinging URL: " + this.pS);
            }
            httpURLConnection.disconnect();
        } catch (IOException e) {
            da.w("Error while pinging URL: " + this.pS + ". " + e.getMessage());
        } catch (Throwable th) {
            httpURLConnection.disconnect();
        }
    }

    public void onStop() {
    }
}
