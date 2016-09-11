package com.google.ads.mediation.jsadapter;

import android.text.TextUtils;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import com.google.android.gms.internal.da;
import java.net.URI;
import java.net.URISyntaxException;

public final class BannerWebViewClient extends WebViewClient {
    private final String A;
    private boolean B;
    private final JavascriptAdapter r;

    public BannerWebViewClient(JavascriptAdapter adapter, String passbackUrl) {
        this.A = c(passbackUrl);
        this.r = adapter;
        this.B = false;
    }

    private boolean b(String str) {
        Object c = c(str);
        if (TextUtils.isEmpty(c)) {
            return false;
        }
        try {
            URI uri = new URI(c);
            if ("passback".equals(uri.getScheme())) {
                da.s("Passback received");
                this.r.sendAdNotReceivedUpdate();
                return true;
            } else if (TextUtils.isEmpty(this.A)) {
                return false;
            } else {
                URI uri2 = new URI(this.A);
                String host = uri2.getHost();
                String host2 = uri.getHost();
                String path = uri2.getPath();
                String path2 = uri.getPath();
                if (!equals(host, host2) || !equals(path, path2)) {
                    return false;
                }
                da.s("Passback received");
                this.r.sendAdNotReceivedUpdate();
                return true;
            }
        } catch (URISyntaxException e) {
            da.t(e.getMessage());
            return false;
        }
    }

    private String c(String str) {
        if (!TextUtils.isEmpty(str)) {
            try {
                if (str.endsWith("/")) {
                    str = str.substring(0, str.length() - 1);
                }
            } catch (IndexOutOfBoundsException e) {
                da.t(e.getMessage());
            }
        }
        return str;
    }

    private static boolean equals(Object obj1, Object obj2) {
        return obj1 == obj2 || (obj1 != null && obj1.equals(obj2));
    }

    public void onLoadResource(WebView view, String url) {
        da.v("onLoadResource: " + url);
        if (!b(url)) {
            super.onLoadResource(view, url);
        }
    }

    public void onPageFinished(WebView view, String url) {
        da.v("onPageFinished: " + url);
        super.onPageFinished(view, url);
        if (!this.B) {
            this.r.startCheckingForAd();
            this.B = true;
        }
    }

    public boolean shouldOverrideUrlLoading(WebView view, String url) {
        da.v("shouldOverrideUrlLoading: " + url);
        if (!b(url)) {
            return false;
        }
        da.s("shouldOverrideUrlLoading: received passback url");
        return true;
    }
}
