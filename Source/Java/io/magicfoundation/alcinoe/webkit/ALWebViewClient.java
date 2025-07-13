package io.magicfoundation.alcinoe.webkit;

import android.graphics.Bitmap;
import android.net.http.SslError;
import android.os.Message;
import android.view.KeyEvent;
import android.webkit.HttpAuthHandler;
import android.webkit.SslErrorHandler;
import android.webkit.WebResourceError;
import android.webkit.WebResourceRequest;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import androidx.annotation.Nullable;

public class ALWebViewClient extends WebViewClient {
    
    @Nullable
    private ALWebViewListener mListener;

    public void setWebViewListener(@Nullable ALWebViewListener listener) {
        mListener = listener;
    }

    @Override
    public void doUpdateVisitedHistory(WebView view, String url, boolean isReload) {
        super.doUpdateVisitedHistory(view, url, isReload);
        if (mListener != null) {
            mListener.doUpdateVisitedHistory(view, url, isReload);
        }
    }

    @Override
    public void onFormResubmission(WebView view, Message dontResend, Message resend) {
        super.onFormResubmission(view, dontResend, resend);
        if (mListener != null) {
            mListener.onFormResubmission(view, dontResend, resend);
        }
    }

    @Override
    public void onLoadResource(WebView view, String url) {
        super.onLoadResource(view, url);
        if (mListener != null) {
            mListener.onLoadResource(view, url);
        }
    }

    @Override
    public void onPageFinished(WebView view, String url) {
        super.onPageFinished(view, url);
        if (mListener != null) {
            mListener.onPageFinished(view, url);
        }
    }

    @Override
    public void onPageStarted(WebView view, String url, Bitmap favicon) {
        super.onPageStarted(view, url, favicon);
        if (mListener != null) {
            mListener.onPageStarted(view, url, favicon);
        }
    }

    @Override
    public void onReceivedError(WebView view, WebResourceRequest request, WebResourceError error) {
        super.onReceivedError(view, request, error);
        if (mListener != null) {
            mListener.onReceivedError(
                view,
                error.getErrorCode(),
                error.getDescription() != null ? error.getDescription().toString() : "",
                request.getUrl() != null ? request.getUrl().toString() : "");
        }
    }

    @SuppressWarnings("deprecation")
    @Override
    public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
        super.onReceivedError(view, errorCode, description, failingUrl);
        if (mListener != null) {
            mListener.onReceivedError(view, errorCode, description, failingUrl);
        }
    }

    @Override
    public void onReceivedHttpAuthRequest(WebView view, HttpAuthHandler handler, String host, String realm) {
        super.onReceivedHttpAuthRequest(view, handler, host, realm);
        if (mListener != null) {
            mListener.onReceivedHttpAuthRequest(view, handler, host, realm);
        }
    }

    @Override
    public void onReceivedSslError(WebView view, SslErrorHandler handler, SslError error) {
        super.onReceivedSslError(view, handler, error);
        if (mListener != null) {
            mListener.onReceivedSslError(view, handler, error);
        }
    }

    @Override
    public void onScaleChanged(WebView view, float oldScale, float newScale) {
        super.onScaleChanged(view, oldScale, newScale);
        if (mListener != null) {
            mListener.onScaleChanged(view, oldScale, newScale);
        }
    }

    @Override
    public void onUnhandledKeyEvent(WebView view, KeyEvent event) {
        super.onUnhandledKeyEvent(view, event);
        if (mListener != null) {
            mListener.onUnhandledKeyEvent(view, event);
        }
    }

    @Override
    public boolean shouldOverrideKeyEvent(WebView view, KeyEvent event) {
        if (mListener != null) {
            mListener.shouldOverrideKeyEvent(view, event);
        }
        return super.shouldOverrideKeyEvent(view, event);
    }

    @Override
    public boolean shouldOverrideUrlLoading(WebView view, WebResourceRequest request) {
        if (mListener == null || request == null || request.getUrl() == null) {
            return super.shouldOverrideUrlLoading(view, request);
        } else {
            return mListener.shouldOverrideUrlLoading(view, request.getUrl().toString());
        }
    }

    @SuppressWarnings("deprecation")
    @Override
    public boolean shouldOverrideUrlLoading(WebView view, String url) {
        if (mListener == null) {
            return super.shouldOverrideUrlLoading(view, url);
        } else {
            return mListener.shouldOverrideUrlLoading(view, url);
        }
    }
}