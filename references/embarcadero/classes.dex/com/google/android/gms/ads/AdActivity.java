package com.google.android.gms.ads;

import android.app.Activity;
import android.os.Bundle;
import com.google.android.gms.internal.bv;
import com.google.android.gms.internal.bw;
import com.google.android.gms.internal.da;

public final class AdActivity extends Activity {
    public static final String CLASS_NAME = "com.google.android.gms.ads.AdActivity";
    public static final String SIMPLE_CLASS_NAME = "AdActivity";
    private bw kz;

    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        this.kz = bv.a(this);
        if (this.kz == null) {
            da.w("Could not create ad overlay.");
            finish();
            return;
        }
        try {
            this.kz.onCreate(savedInstanceState);
        } catch (Throwable e) {
            da.b("Could not forward onCreate to ad overlay:", e);
            finish();
        }
    }

    protected void onDestroy() {
        try {
            if (this.kz != null) {
                this.kz.onDestroy();
            }
        } catch (Throwable e) {
            da.b("Could not forward onDestroy to ad overlay:", e);
        }
        super.onDestroy();
    }

    protected void onPause() {
        try {
            if (this.kz != null) {
                this.kz.onPause();
            }
        } catch (Throwable e) {
            da.b("Could not forward onPause to ad overlay:", e);
            finish();
        }
        super.onPause();
    }

    protected void onRestart() {
        super.onRestart();
        try {
            if (this.kz != null) {
                this.kz.onRestart();
            }
        } catch (Throwable e) {
            da.b("Could not forward onRestart to ad overlay:", e);
            finish();
        }
    }

    protected void onResume() {
        super.onResume();
        try {
            if (this.kz != null) {
                this.kz.onResume();
            }
        } catch (Throwable e) {
            da.b("Could not forward onResume to ad overlay:", e);
            finish();
        }
    }

    protected void onSaveInstanceState(Bundle outState) {
        try {
            if (this.kz != null) {
                this.kz.onSaveInstanceState(outState);
            }
        } catch (Throwable e) {
            da.b("Could not forward onSaveInstanceState to ad overlay:", e);
            finish();
        }
        super.onSaveInstanceState(outState);
    }

    protected void onStart() {
        super.onStart();
        try {
            if (this.kz != null) {
                this.kz.onStart();
            }
        } catch (Throwable e) {
            da.b("Could not forward onStart to ad overlay:", e);
            finish();
        }
    }

    protected void onStop() {
        try {
            if (this.kz != null) {
                this.kz.onStop();
            }
        } catch (Throwable e) {
            da.b("Could not forward onStop to ad overlay:", e);
            finish();
        }
        super.onStop();
    }
}
