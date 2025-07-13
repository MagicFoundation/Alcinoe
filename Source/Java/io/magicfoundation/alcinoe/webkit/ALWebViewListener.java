package io.magicfoundation.alcinoe.webkit;

import android.graphics.Bitmap;
import android.net.http.SslError;
import android.os.Message;
import android.view.KeyEvent;
import android.webkit.HttpAuthHandler;
import android.webkit.SslErrorHandler;
import android.webkit.WebView;

public interface ALWebViewListener {
	public void doUpdateVisitedHistory(WebView view, String url, boolean isReload) ;
	public void onFormResubmission(WebView view, Message dontResend, Message resend);
	public void onLoadResource(WebView view, String url);	
	public void onPageFinished(WebView view, String url);
	public void onPageStarted(WebView view, String url, Bitmap favicon);
	public void onReceivedError(WebView view, int errorCode, String description, String failingUrl);
	public void onReceivedHttpAuthRequest(WebView view, HttpAuthHandler handler, String host, String realm);
	public void onReceivedSslError(WebView view, SslErrorHandler handler, SslError error);
	public void onScaleChanged(WebView view, float oldScale, float newScale);
	public void onUnhandledKeyEvent(WebView view, KeyEvent event);
	public boolean shouldOverrideKeyEvent(WebView view, KeyEvent event);
	public boolean shouldOverrideUrlLoading(WebView view, String url);
}