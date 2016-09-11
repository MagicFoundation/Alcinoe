package com.google.android.gms.internal;

import android.graphics.Canvas;
import android.net.Uri;
import android.widget.ImageView;

public final class ec extends ImageView {
    private Uri Bl;
    private int Bm;
    private int Bn;

    public void N(int i) {
        this.Bm = i;
    }

    public void d(Uri uri) {
        this.Bl = uri;
    }

    public int dQ() {
        return this.Bm;
    }

    protected void onDraw(Canvas canvas) {
        super.onDraw(canvas);
        if (this.Bn != 0) {
            canvas.drawColor(this.Bn);
        }
    }
}
