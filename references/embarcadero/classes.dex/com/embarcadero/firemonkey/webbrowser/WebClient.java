package com.embarcadero.firemonkey.webbrowser;

import android.graphics.Bitmap;
import android.net.http.SslError;
import android.os.Message;
import android.view.KeyEvent;
import android.webkit.HttpAuthHandler;
import android.webkit.SslErrorHandler;
import android.webkit.WebView;
import android.webkit.WebViewClient;

public class WebClient extends WebViewClient {
    private OnWebViewListener mListener;

    public void SetWebViewListener(OnWebViewListener listener) {
        this.mListener = listener;
    }

    public void doUpdateVisitedHistory(WebView view, String url, boolean isReload) {
        super.doUpdateVisitedHistory(view, url, isReload);
        if (this.mListener != null) {
            this.mListener.doUpdateVisitedHistory(view, url, isReload);
        }
    }

    public void onFormResubmission(WebView view, Message dontResend, Message resend) {
        super.onFormResubmission(view, dontResend, resend);
        if (this.mListener != null) {
            this.mListener.onFormResubmission(view, dontResend, resend);
        }
    }

    public void onLoadResource(WebView view, String url) {
        super.onLoadResource(view, url);
        if (this.mListener != null) {
            this.mListener.onLoadResource(view, url);
        }
    }

    public void onPageFinished(WebView view, String url) {
        super.onPageFinished(view, url);
        if (this.mListener != null) {
            this.mListener.onPageFinished(view, url);
        }
    }

    public void onPageStarted(WebView view, String url, Bitmap favicon) {
        super.onPageStarted(view, url, favicon);
        if (this.mListener != null) {
            this.mListener.onPageStarted(view, url, favicon);
        }
    }

    public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
        super.onReceivedError(view, errorCode, description, failingUrl);
        if (this.mListener != null) {
            this.mListener.onReceivedError(view, errorCode, description, failingUrl);
        }
    }

    public void onReceivedHttpAuthRequest(WebView view, HttpAuthHandler handler, String host, String realm) {
        super.onReceivedHttpAuthRequest(view, handler, host, realm);
        if (this.mListener != null) {
            this.mListener.onReceivedHttpAuthRequest(view, handler, host, realm);
        }
    }

    public void onReceivedSslError(WebView view, SslErrorHandler handler, SslError error) {
        super.onReceivedSslError(view, handler, error);
        if (this.mListener != null) {
            this.mListener.onReceivedSslError(view, handler, error);
        }
    }

    public void onScaleChanged(WebView view, float oldScale, float newScale) {
        super.onScaleChanged(view, oldScale, newScale);
        if (this.mListener != null) {
            this.mListener.onScaleChanged(view, oldScale, newScale);
        }
    }

    public void onUnhandledKeyEvent(WebView view, KeyEvent event) {
        super.onUnhandledKeyEvent(view, event);
        if (this.mListener != null) {
            this.mListener.onUnhandledKeyEvent(view, event);
        }
    }

    public boolean shouldOverrideKeyEvent(WebView view, KeyEvent event) {
        if (this.mListener != null) {
            this.mListener.shouldOverrideKeyEvent(view, event);
        }
        return super.shouldOverrideKeyEvent(view, event);
    }

    public boolean shouldOverrideUrlLoading(WebView view, String url) {
        if (url.startsWith("about:")) {
            return false;
        }
        view.loadUrl(url);
        if (this.mListener != null) {
            this.mListener.shouldOverrideUrlLoading(view, url);
        }
        return true;
    }
}
