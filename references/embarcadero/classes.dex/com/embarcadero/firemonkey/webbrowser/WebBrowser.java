package com.embarcadero.firemonkey.webbrowser;

import android.content.Context;
import android.webkit.WebChromeClient;
import android.webkit.WebView;

public class WebBrowser extends WebView {
    private WebChromeClient mWebChromeClient;
    private WebClient mWebClient;

    public WebBrowser(Context context) {
        super(context);
        this.mWebClient = new WebClient();
        setWebViewClient(this.mWebClient);
        this.mWebChromeClient = new WebChromeClient();
        setWebChromeClient(this.mWebChromeClient);
        setFocusable(true);
    }

    public void SetWebViewListener(OnWebViewListener listener) {
        this.mWebClient.SetWebViewListener(listener);
    }
}
