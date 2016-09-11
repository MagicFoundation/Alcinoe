package com.google.android.gms.dynamic;

import android.app.Activity;
import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.FrameLayout.LayoutParams;
import android.widget.LinearLayout;
import android.widget.TextView;
import com.google.android.gms.common.GooglePlayServicesUtil;
import java.util.Iterator;
import java.util.LinkedList;

public abstract class a<T extends LifecycleDelegate> {
    private T Fp;
    private Bundle Fq;
    private LinkedList<a> Fr;
    private final d<T> Fs;

    /* renamed from: com.google.android.gms.dynamic.a.5 */
    static class AnonymousClass5 implements OnClickListener {
        final /* synthetic */ int FA;
        final /* synthetic */ Context os;

        AnonymousClass5(Context context, int i) {
            this.os = context;
            this.FA = i;
        }

        public void onClick(View v) {
            this.os.startActivity(GooglePlayServicesUtil.a(this.os, this.FA, -1));
        }
    }

    private interface a {
        void b(LifecycleDelegate lifecycleDelegate);

        int getState();
    }

    /* renamed from: com.google.android.gms.dynamic.a.2 */
    class AnonymousClass2 implements a {
        final /* synthetic */ a Ft;
        final /* synthetic */ Activity Fu;
        final /* synthetic */ Bundle Fv;
        final /* synthetic */ Bundle Fw;

        AnonymousClass2(a aVar, Activity activity, Bundle bundle, Bundle bundle2) {
            this.Ft = aVar;
            this.Fu = activity;
            this.Fv = bundle;
            this.Fw = bundle2;
        }

        public void b(LifecycleDelegate lifecycleDelegate) {
            this.Ft.Fp.onInflate(this.Fu, this.Fv, this.Fw);
        }

        public int getState() {
            return 0;
        }
    }

    /* renamed from: com.google.android.gms.dynamic.a.3 */
    class AnonymousClass3 implements a {
        final /* synthetic */ a Ft;
        final /* synthetic */ Bundle Fw;

        AnonymousClass3(a aVar, Bundle bundle) {
            this.Ft = aVar;
            this.Fw = bundle;
        }

        public void b(LifecycleDelegate lifecycleDelegate) {
            this.Ft.Fp.onCreate(this.Fw);
        }

        public int getState() {
            return 1;
        }
    }

    /* renamed from: com.google.android.gms.dynamic.a.4 */
    class AnonymousClass4 implements a {
        final /* synthetic */ a Ft;
        final /* synthetic */ Bundle Fw;
        final /* synthetic */ FrameLayout Fx;
        final /* synthetic */ LayoutInflater Fy;
        final /* synthetic */ ViewGroup Fz;

        AnonymousClass4(a aVar, FrameLayout frameLayout, LayoutInflater layoutInflater, ViewGroup viewGroup, Bundle bundle) {
            this.Ft = aVar;
            this.Fx = frameLayout;
            this.Fy = layoutInflater;
            this.Fz = viewGroup;
            this.Fw = bundle;
        }

        public void b(LifecycleDelegate lifecycleDelegate) {
            this.Fx.removeAllViews();
            this.Fx.addView(this.Ft.Fp.onCreateView(this.Fy, this.Fz, this.Fw));
        }

        public int getState() {
            return 2;
        }
    }

    public a() {
        this.Fs = new d<T>() {
            final /* synthetic */ a Ft;

            {
                this.Ft = r1;
            }

            public void a(T t) {
                this.Ft.Fp = t;
                Iterator it = this.Ft.Fr.iterator();
                while (it.hasNext()) {
                    ((a) it.next()).b(this.Ft.Fp);
                }
                this.Ft.Fr.clear();
                this.Ft.Fq = null;
            }
        };
    }

    private void a(Bundle bundle, a aVar) {
        if (this.Fp != null) {
            aVar.b(this.Fp);
            return;
        }
        if (this.Fr == null) {
            this.Fr = new LinkedList();
        }
        this.Fr.add(aVar);
        if (bundle != null) {
            if (this.Fq == null) {
                this.Fq = (Bundle) bundle.clone();
            } else {
                this.Fq.putAll(bundle);
            }
        }
        a(this.Fs);
    }

    private void aO(int i) {
        while (!this.Fr.isEmpty() && ((a) this.Fr.getLast()).getState() >= i) {
            this.Fr.removeLast();
        }
    }

    public static void b(FrameLayout frameLayout) {
        Context context = frameLayout.getContext();
        int isGooglePlayServicesAvailable = GooglePlayServicesUtil.isGooglePlayServicesAvailable(context);
        CharSequence b = GooglePlayServicesUtil.b(context, isGooglePlayServicesAvailable, -1);
        CharSequence b2 = GooglePlayServicesUtil.b(context, isGooglePlayServicesAvailable);
        View linearLayout = new LinearLayout(frameLayout.getContext());
        linearLayout.setOrientation(1);
        linearLayout.setLayoutParams(new LayoutParams(-2, -2));
        frameLayout.addView(linearLayout);
        View textView = new TextView(frameLayout.getContext());
        textView.setLayoutParams(new LayoutParams(-2, -2));
        textView.setText(b);
        linearLayout.addView(textView);
        if (b2 != null) {
            View button = new Button(context);
            button.setLayoutParams(new LayoutParams(-2, -2));
            button.setText(b2);
            linearLayout.addView(button);
            button.setOnClickListener(new AnonymousClass5(context, isGooglePlayServicesAvailable));
        }
    }

    protected void a(FrameLayout frameLayout) {
        b(frameLayout);
    }

    protected abstract void a(d<T> dVar);

    public T fj() {
        return this.Fp;
    }

    public void onCreate(Bundle savedInstanceState) {
        a(savedInstanceState, new AnonymousClass3(this, savedInstanceState));
    }

    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        FrameLayout frameLayout = new FrameLayout(inflater.getContext());
        a(savedInstanceState, new AnonymousClass4(this, frameLayout, inflater, container, savedInstanceState));
        if (this.Fp == null) {
            a(frameLayout);
        }
        return frameLayout;
    }

    public void onDestroy() {
        if (this.Fp != null) {
            this.Fp.onDestroy();
        } else {
            aO(1);
        }
    }

    public void onDestroyView() {
        if (this.Fp != null) {
            this.Fp.onDestroyView();
        } else {
            aO(2);
        }
    }

    public void onInflate(Activity activity, Bundle attrs, Bundle savedInstanceState) {
        a(savedInstanceState, new AnonymousClass2(this, activity, attrs, savedInstanceState));
    }

    public void onLowMemory() {
        if (this.Fp != null) {
            this.Fp.onLowMemory();
        }
    }

    public void onPause() {
        if (this.Fp != null) {
            this.Fp.onPause();
        } else {
            aO(5);
        }
    }

    public void onResume() {
        a(null, new a() {
            final /* synthetic */ a Ft;

            {
                this.Ft = r1;
            }

            public void b(LifecycleDelegate lifecycleDelegate) {
                this.Ft.Fp.onResume();
            }

            public int getState() {
                return 5;
            }
        });
    }

    public void onSaveInstanceState(Bundle outState) {
        if (this.Fp != null) {
            this.Fp.onSaveInstanceState(outState);
        } else if (this.Fq != null) {
            outState.putAll(this.Fq);
        }
    }
}
