package io.magicfoundation.alcinoe.webkit;

import android.content.Context;
import android.webkit.JavascriptInterface;
import android.webkit.WebView;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class ALWebView extends WebView {
    
    private static final String JAVASCRIPT_INTERFACE_NAME = "alcinoeNativeBridge";
    
    @NonNull
    private final ALWebViewClient mWebViewClient;

    @Nullable
    private volatile ALWebViewListener mListener;

    public ALWebView(@NonNull Context context) {
        super(context);
        mWebViewClient = new ALWebViewClient();
        setWebViewClient(mWebViewClient);
        addJavascriptInterface(new JavascriptBridge(), JAVASCRIPT_INTERFACE_NAME);
    }
    
    @Override
    public void destroy() {
        removeJavascriptInterface(JAVASCRIPT_INTERFACE_NAME);
        super.destroy();
    }

    public void setListener(@Nullable ALWebViewListener listener) {
        mListener = listener;
        mWebViewClient.setWebViewListener(listener);
    }
    
    private final class JavascriptBridge {
        @JavascriptInterface
        public void postMessage(@NonNull String message) {
            // Methods annotated with @JavascriptInterface are called on a private
            // background thread ("JavaBridge"), not on the UI thread.
            post(() -> {
                ALWebViewListener listener = mListener;
                if (listener != null) {
                    listener.onWebMessage(ALWebView.this, message);
                }
            });
        }
    }
    
}