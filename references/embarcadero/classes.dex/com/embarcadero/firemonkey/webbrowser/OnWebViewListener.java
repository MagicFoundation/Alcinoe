package com.embarcadero.firemonkey.webbrowser;

import android.graphics.Bitmap;
import android.net.http.SslError;
import android.os.Message;
import android.view.KeyEvent;
import android.webkit.HttpAuthHandler;
import android.webkit.SslErrorHandler;
import android.webkit.WebView;

public interface OnWebViewListener {
    void doUpdateVisitedHistory(WebView webView, String str, boolean z);

    void onFormResubmission(WebView webView, Message message, Message message2);

    void onLoadResource(WebView webView, String str);

    void onPageFinished(WebView webView, String str);

    void onPageStarted(WebView webView, String str, Bitmap bitmap);

    void onReceivedError(WebView webView, int i, String str, String str2);

    void onReceivedHttpAuthRequest(WebView webView, HttpAuthHandler httpAuthHandler, String str, String str2);

    void onReceivedSslError(WebView webView, SslErrorHandler sslErrorHandler, SslError sslError);

    void onScaleChanged(WebView webView, float f, float f2);

    void onUnhandledKeyEvent(WebView webView, KeyEvent keyEvent);

    boolean shouldOverrideKeyEvent(WebView webView, KeyEvent keyEvent);

    boolean shouldOverrideUrlLoading(WebView webView, String str);
}
