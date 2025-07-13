package io.magicfoundation.alcinoe.webkit;

import android.content.Context;
import android.webkit.WebView;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class ALWebView extends WebView {
    
    @NonNull
    private final ALWebViewClient mWebViewClient;

    public ALWebView(@NonNull Context context) {
        super(context);
        mWebViewClient = new ALWebViewClient();
        setWebViewClient(mWebViewClient);
    }

    public void setListener(@Nullable ALWebViewListener listener) {
        mWebViewClient.setWebViewListener(listener);
    }
    
}