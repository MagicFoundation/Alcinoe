package com.google.android.gms.internal;

import android.content.Context;
import android.webkit.WebResourceResponse;
import android.webkit.WebView;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

public final class dg extends de {
    public dg(dd ddVar, boolean z) {
        super(ddVar, z);
    }

    private static WebResourceResponse d(Context context, String str, String str2) throws IOException {
        HttpURLConnection httpURLConnection = (HttpURLConnection) new URL(str2).openConnection();
        try {
            cv.a(context, str, true, httpURLConnection);
            httpURLConnection.connect();
            WebResourceResponse webResourceResponse = new WebResourceResponse("application/javascript", "UTF-8", new ByteArrayInputStream(cv.a(new InputStreamReader(httpURLConnection.getInputStream())).getBytes("UTF-8")));
            return webResourceResponse;
        } finally {
            httpURLConnection.disconnect();
        }
    }

    public WebResourceResponse shouldInterceptRequest(WebView webView, String url) {
        try {
            if (!"mraid.js".equalsIgnoreCase(new File(url).getName())) {
                return super.shouldInterceptRequest(webView, url);
            }
            if (webView instanceof dd) {
                dd ddVar = (dd) webView;
                ddVar.bb().ar();
                if (ddVar.Q().lo) {
                    da.v("shouldInterceptRequest(http://media.admob.com/mraid/v1/mraid_app_interstitial.js)");
                    return d(ddVar.getContext(), this.ng.bd().pU, "http://media.admob.com/mraid/v1/mraid_app_interstitial.js");
                } else if (ddVar.be()) {
                    da.v("shouldInterceptRequest(http://media.admob.com/mraid/v1/mraid_app_expanded_banner.js)");
                    return d(ddVar.getContext(), this.ng.bd().pU, "http://media.admob.com/mraid/v1/mraid_app_expanded_banner.js");
                } else {
                    da.v("shouldInterceptRequest(http://media.admob.com/mraid/v1/mraid_app_banner.js)");
                    return d(ddVar.getContext(), this.ng.bd().pU, "http://media.admob.com/mraid/v1/mraid_app_banner.js");
                }
            }
            da.w("Tried to intercept request from a WebView that wasn't an AdWebView.");
            return super.shouldInterceptRequest(webView, url);
        } catch (IOException e) {
            da.w("Could not fetching MRAID JS. " + e.getMessage());
            return super.shouldInterceptRequest(webView, url);
        }
    }
}
