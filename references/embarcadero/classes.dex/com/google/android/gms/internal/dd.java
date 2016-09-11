package com.google.android.gms.internal;

import android.app.Activity;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.content.MutableContextWrapper;
import android.net.Uri;
import android.os.Build.VERSION;
import android.support.v4.widget.ExploreByTouchHelper;
import android.view.MotionEvent;
import android.view.View.MeasureSpec;
import android.webkit.DownloadListener;
import android.webkit.WebSettings;
import android.webkit.WebView;
import com.google.android.gms.drive.DriveFile;
import java.util.HashMap;
import java.util.Map;
import org.json.JSONException;

public final class dd extends WebView implements DownloadListener {
    private ab mF;
    private final db mG;
    private final Object mg;
    private final l nP;
    private final de pY;
    private final a pZ;
    private bo qa;
    private boolean qb;
    private boolean qc;

    private static class a extends MutableContextWrapper {
        private Activity qd;
        private Context qe;

        public a(Context context) {
            super(context);
            setBaseContext(context);
        }

        public void setBaseContext(Context base) {
            this.qe = base.getApplicationContext();
            this.qd = base instanceof Activity ? (Activity) base : null;
            super.setBaseContext(this.qe);
        }

        public void startActivity(Intent intent) {
            if (this.qd != null) {
                this.qd.startActivity(intent);
                return;
            }
            intent.setFlags(DriveFile.MODE_READ_ONLY);
            this.qe.startActivity(intent);
        }
    }

    private dd(a aVar, ab abVar, boolean z, boolean z2, l lVar, db dbVar) {
        super(aVar);
        this.mg = new Object();
        this.pZ = aVar;
        this.mF = abVar;
        this.qb = z;
        this.nP = lVar;
        this.mG = dbVar;
        setBackgroundColor(0);
        WebSettings settings = getSettings();
        settings.setJavaScriptEnabled(true);
        settings.setSavePassword(false);
        settings.setSupportMultipleWindows(true);
        settings.setJavaScriptCanOpenWindowsAutomatically(true);
        cv.a((Context) aVar, dbVar.pU, settings);
        if (VERSION.SDK_INT >= 17) {
            cx.a(getContext(), settings);
        } else if (VERSION.SDK_INT >= 11) {
            cw.a(getContext(), settings);
        }
        setDownloadListener(this);
        if (VERSION.SDK_INT >= 11) {
            this.pY = new dg(this, z2);
        } else {
            this.pY = new de(this, z2);
        }
        setWebViewClient(this.pY);
        if (VERSION.SDK_INT >= 14) {
            setWebChromeClient(new dh(this));
        } else if (VERSION.SDK_INT >= 11) {
            setWebChromeClient(new df(this));
        }
        bf();
    }

    public static dd a(Context context, ab abVar, boolean z, boolean z2, l lVar, db dbVar) {
        return new dd(new a(context), abVar, z, z2, lVar, dbVar);
    }

    private void bf() {
        synchronized (this.mg) {
            if (this.qb || this.mF.lo) {
                if (VERSION.SDK_INT < 14) {
                    da.s("Disabling hardware acceleration on an overlay.");
                    bg();
                } else {
                    da.s("Enabling hardware acceleration on an overlay.");
                    bh();
                }
            } else if (VERSION.SDK_INT < 18) {
                da.s("Disabling hardware acceleration on an AdView.");
                bg();
            } else {
                da.s("Enabling hardware acceleration on an AdView.");
                bh();
            }
        }
    }

    private void bg() {
        synchronized (this.mg) {
            if (!this.qc && VERSION.SDK_INT >= 11) {
                cw.c(this);
            }
            this.qc = true;
        }
    }

    private void bh() {
        synchronized (this.mg) {
            if (this.qc && VERSION.SDK_INT >= 11) {
                cw.d(this);
            }
            this.qc = false;
        }
    }

    public ab Q() {
        ab abVar;
        synchronized (this.mg) {
            abVar = this.mF;
        }
        return abVar;
    }

    public void a(Context context, ab abVar) {
        synchronized (this.mg) {
            this.pZ.setBaseContext(context);
            this.qa = null;
            this.mF = abVar;
            this.qb = false;
            cv.b(this);
            loadUrl("about:blank");
            this.pY.reset();
        }
    }

    public void a(ab abVar) {
        synchronized (this.mg) {
            this.mF = abVar;
            requestLayout();
        }
    }

    public void a(bo boVar) {
        synchronized (this.mg) {
            this.qa = boVar;
        }
    }

    public void a(String str, Map<String, ?> map) {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("javascript:AFMA_ReceiveMessage('");
        stringBuilder.append(str);
        stringBuilder.append("'");
        if (map != null) {
            try {
                String jSONObject = cv.m((Map) map).toString();
                stringBuilder.append(",");
                stringBuilder.append(jSONObject);
            } catch (JSONException e) {
                da.w("Could not convert AFMA event parameters to JSON.");
                return;
            }
        }
        stringBuilder.append(");");
        da.v("Dispatching AFMA event: " + stringBuilder);
        loadUrl(stringBuilder.toString());
    }

    public void aY() {
        Map hashMap = new HashMap(1);
        hashMap.put("version", this.mG.pU);
        a("onhide", hashMap);
    }

    public void aZ() {
        Map hashMap = new HashMap(1);
        hashMap.put("version", this.mG.pU);
        a("onshow", hashMap);
    }

    public bo ba() {
        bo boVar;
        synchronized (this.mg) {
            boVar = this.qa;
        }
        return boVar;
    }

    public de bb() {
        return this.pY;
    }

    public l bc() {
        return this.nP;
    }

    public db bd() {
        return this.mG;
    }

    public boolean be() {
        boolean z;
        synchronized (this.mg) {
            z = this.qb;
        }
        return z;
    }

    public void n(boolean z) {
        synchronized (this.mg) {
            this.qb = z;
            bf();
        }
    }

    public void onDownloadStart(String url, String userAgent, String contentDisposition, String mimeType, long size) {
        try {
            Intent intent = new Intent("android.intent.action.VIEW");
            intent.setDataAndType(Uri.parse(url), mimeType);
            getContext().startActivity(intent);
        } catch (ActivityNotFoundException e) {
            da.s("Couldn't find an Activity to view url/mimetype: " + url + " / " + mimeType);
        }
    }

    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        int i = Integer.MAX_VALUE;
        synchronized (this.mg) {
            if (isInEditMode() || this.qb) {
                super.onMeasure(widthMeasureSpec, heightMeasureSpec);
                return;
            }
            int mode = MeasureSpec.getMode(widthMeasureSpec);
            int size = MeasureSpec.getSize(widthMeasureSpec);
            int mode2 = MeasureSpec.getMode(heightMeasureSpec);
            int size2 = MeasureSpec.getSize(heightMeasureSpec);
            mode = (mode == ExploreByTouchHelper.INVALID_ID || mode == 1073741824) ? size : Integer.MAX_VALUE;
            if (mode2 == ExploreByTouchHelper.INVALID_ID || mode2 == 1073741824) {
                i = size2;
            }
            if (this.mF.widthPixels > mode || this.mF.heightPixels > r0) {
                da.w("Not enough space to show ad. Needs " + this.mF.widthPixels + "x" + this.mF.heightPixels + " pixels, but only has " + size + "x" + size2 + " pixels.");
                if (getVisibility() != 8) {
                    setVisibility(4);
                }
                setMeasuredDimension(0, 0);
            } else {
                if (getVisibility() != 8) {
                    setVisibility(0);
                }
                setMeasuredDimension(this.mF.widthPixels, this.mF.heightPixels);
            }
        }
    }

    public boolean onTouchEvent(MotionEvent event) {
        if (this.nP != null) {
            this.nP.a(event);
        }
        return super.onTouchEvent(event);
    }

    public void setContext(Context context) {
        this.pZ.setBaseContext(context);
    }
}
