package com.google.android.gms.maps;

import android.app.Activity;
import android.content.Context;
import android.os.Bundle;
import android.os.RemoteException;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import com.google.android.gms.common.GooglePlayServicesNotAvailableException;
import com.google.android.gms.dynamic.LifecycleDelegate;
import com.google.android.gms.dynamic.c;
import com.google.android.gms.dynamic.d;
import com.google.android.gms.internal.er;
import com.google.android.gms.maps.internal.IMapViewDelegate;
import com.google.android.gms.maps.internal.q;
import com.google.android.gms.maps.model.RuntimeRemoteException;

public class MapView extends FrameLayout {
    private GoogleMap Pn;
    private final b Pr;

    static class a implements LifecycleDelegate {
        private final ViewGroup Ps;
        private final IMapViewDelegate Pt;
        private View Pu;

        public a(ViewGroup viewGroup, IMapViewDelegate iMapViewDelegate) {
            this.Pt = (IMapViewDelegate) er.f(iMapViewDelegate);
            this.Ps = (ViewGroup) er.f(viewGroup);
        }

        public IMapViewDelegate gX() {
            return this.Pt;
        }

        public void onCreate(Bundle savedInstanceState) {
            try {
                this.Pt.onCreate(savedInstanceState);
                this.Pu = (View) c.b(this.Pt.getView());
                this.Ps.removeAllViews();
                this.Ps.addView(this.Pu);
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }

        public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
            throw new UnsupportedOperationException("onCreateView not allowed on MapViewDelegate");
        }

        public void onDestroy() {
            try {
                this.Pt.onDestroy();
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }

        public void onDestroyView() {
            throw new UnsupportedOperationException("onDestroyView not allowed on MapViewDelegate");
        }

        public void onInflate(Activity activity, Bundle attrs, Bundle savedInstanceState) {
            throw new UnsupportedOperationException("onInflate not allowed on MapViewDelegate");
        }

        public void onLowMemory() {
            try {
                this.Pt.onLowMemory();
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }

        public void onPause() {
            try {
                this.Pt.onPause();
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }

        public void onResume() {
            try {
                this.Pt.onResume();
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }

        public void onSaveInstanceState(Bundle outState) {
            try {
                this.Pt.onSaveInstanceState(outState);
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }

        public void onStart() {
        }

        public void onStop() {
        }
    }

    static class b extends com.google.android.gms.dynamic.a<a> {
        protected d<a> Pq;
        private final ViewGroup Pv;
        private final GoogleMapOptions Pw;
        private final Context mContext;

        b(ViewGroup viewGroup, Context context, GoogleMapOptions googleMapOptions) {
            this.Pv = viewGroup;
            this.mContext = context;
            this.Pw = googleMapOptions;
        }

        protected void a(d<a> dVar) {
            this.Pq = dVar;
            gW();
        }

        public void gW() {
            if (this.Pq != null && fj() == null) {
                try {
                    this.Pq.a(new a(this.Pv, q.A(this.mContext).a(c.h(this.mContext), this.Pw)));
                } catch (RemoteException e) {
                    throw new RuntimeRemoteException(e);
                } catch (GooglePlayServicesNotAvailableException e2) {
                }
            }
        }
    }

    public MapView(Context context) {
        super(context);
        this.Pr = new b(this, context, null);
    }

    public MapView(Context context, AttributeSet attrs) {
        super(context, attrs);
        this.Pr = new b(this, context, GoogleMapOptions.createFromAttributes(context, attrs));
    }

    public MapView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        this.Pr = new b(this, context, GoogleMapOptions.createFromAttributes(context, attrs));
    }

    public MapView(Context context, GoogleMapOptions options) {
        super(context);
        this.Pr = new b(this, context, options);
    }

    public final GoogleMap getMap() {
        if (this.Pn != null) {
            return this.Pn;
        }
        this.Pr.gW();
        if (this.Pr.fj() == null) {
            return null;
        }
        try {
            this.Pn = new GoogleMap(((a) this.Pr.fj()).gX().getMap());
            return this.Pn;
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final void onCreate(Bundle savedInstanceState) {
        this.Pr.onCreate(savedInstanceState);
        if (this.Pr.fj() == null) {
            b bVar = this.Pr;
            com.google.android.gms.dynamic.a.b((FrameLayout) this);
        }
    }

    public final void onDestroy() {
        this.Pr.onDestroy();
    }

    public final void onLowMemory() {
        this.Pr.onLowMemory();
    }

    public final void onPause() {
        this.Pr.onPause();
    }

    public final void onResume() {
        this.Pr.onResume();
    }

    public final void onSaveInstanceState(Bundle outState) {
        this.Pr.onSaveInstanceState(outState);
    }
}
