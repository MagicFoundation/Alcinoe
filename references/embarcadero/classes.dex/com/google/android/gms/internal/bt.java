package com.google.android.gms.internal;

import android.app.Activity;
import android.content.Context;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.FrameLayout;
import android.widget.FrameLayout.LayoutParams;
import android.widget.ImageButton;

public final class bt extends FrameLayout implements OnClickListener {
    private final ImageButton nK;
    private final Activity nd;

    public bt(Activity activity, int i) {
        super(activity);
        this.nd = activity;
        setOnClickListener(this);
        this.nK = new ImageButton(activity);
        this.nK.setImageResource(17301527);
        this.nK.setBackgroundColor(0);
        this.nK.setOnClickListener(this);
        this.nK.setPadding(0, 0, 0, 0);
        int a = cz.a((Context) activity, i);
        addView(this.nK, new LayoutParams(a, a, 17));
    }

    public void g(boolean z) {
        this.nK.setVisibility(z ? 4 : 0);
    }

    public void onClick(View view) {
        this.nd.finish();
    }
}
