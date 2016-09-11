package com.google.android.gms.maps;

import android.app.Activity;
import android.app.Fragment;
import android.os.Bundle;
import android.os.Parcelable;
import android.os.RemoteException;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import com.google.android.gms.common.GooglePlayServicesNotAvailableException;
import com.google.android.gms.dynamic.LifecycleDelegate;
import com.google.android.gms.dynamic.c;
import com.google.android.gms.dynamic.d;
import com.google.android.gms.internal.er;
import com.google.android.gms.maps.internal.IGoogleMapDelegate;
import com.google.android.gms.maps.internal.IMapFragmentDelegate;
import com.google.android.gms.maps.internal.p;
import com.google.android.gms.maps.internal.q;
import com.google.android.gms.maps.model.RuntimeRemoteException;

public class MapFragment extends Fragment {
    private final b Pm;
    private GoogleMap Pn;

    static class a implements LifecycleDelegate {
        private final Fragment Po;
        private final IMapFragmentDelegate Pp;

        public a(Fragment fragment, IMapFragmentDelegate iMapFragmentDelegate) {
            this.Pp = (IMapFragmentDelegate) er.f(iMapFragmentDelegate);
            this.Po = (Fragment) er.f(fragment);
        }

        public IMapFragmentDelegate gV() {
            return this.Pp;
        }

        public void onCreate(Bundle savedInstanceState) {
            if (savedInstanceState == null) {
                try {
                    savedInstanceState = new Bundle();
                } catch (RemoteException e) {
                    throw new RuntimeRemoteException(e);
                }
            }
            Bundle arguments = this.Po.getArguments();
            if (arguments != null && arguments.containsKey("MapOptions")) {
                p.a(savedInstanceState, "MapOptions", arguments.getParcelable("MapOptions"));
            }
            this.Pp.onCreate(savedInstanceState);
        }

        public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
            try {
                return (View) c.b(this.Pp.onCreateView(c.h(inflater), c.h(container), savedInstanceState));
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }

        public void onDestroy() {
            try {
                this.Pp.onDestroy();
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }

        public void onDestroyView() {
            try {
                this.Pp.onDestroyView();
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }

        public void onInflate(Activity activity, Bundle attrs, Bundle savedInstanceState) {
            try {
                this.Pp.onInflate(c.h(activity), (GoogleMapOptions) attrs.getParcelable("MapOptions"), savedInstanceState);
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }

        public void onLowMemory() {
            try {
                this.Pp.onLowMemory();
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }

        public void onPause() {
            try {
                this.Pp.onPause();
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }

        public void onResume() {
            try {
                this.Pp.onResume();
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }

        public void onSaveInstanceState(Bundle outState) {
            try {
                this.Pp.onSaveInstanceState(outState);
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
        private final Fragment Po;
        protected d<a> Pq;
        private Activity nd;

        b(Fragment fragment) {
            this.Po = fragment;
        }

        private void setActivity(Activity activity) {
            this.nd = activity;
            gW();
        }

        protected void a(d<a> dVar) {
            this.Pq = dVar;
            gW();
        }

        public void gW() {
            if (this.nd != null && this.Pq != null && fj() == null) {
                try {
                    MapsInitializer.initialize(this.nd);
                    this.Pq.a(new a(this.Po, q.A(this.nd).f(c.h(this.nd))));
                } catch (RemoteException e) {
                    throw new RuntimeRemoteException(e);
                } catch (GooglePlayServicesNotAvailableException e2) {
                }
            }
        }
    }

    public MapFragment() {
        this.Pm = new b(this);
    }

    public static MapFragment newInstance() {
        return new MapFragment();
    }

    public static MapFragment newInstance(GoogleMapOptions options) {
        MapFragment mapFragment = new MapFragment();
        Bundle bundle = new Bundle();
        bundle.putParcelable("MapOptions", options);
        mapFragment.setArguments(bundle);
        return mapFragment;
    }

    protected IMapFragmentDelegate gV() {
        this.Pm.gW();
        return this.Pm.fj() == null ? null : ((a) this.Pm.fj()).gV();
    }

    public final GoogleMap getMap() {
        IMapFragmentDelegate gV = gV();
        if (gV == null) {
            return null;
        }
        try {
            IGoogleMapDelegate map = gV.getMap();
            if (map == null) {
                return null;
            }
            if (this.Pn == null || this.Pn.gM().asBinder() != map.asBinder()) {
                this.Pn = new GoogleMap(map);
            }
            return this.Pn;
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public void onActivityCreated(Bundle savedInstanceState) {
        if (savedInstanceState != null) {
            savedInstanceState.setClassLoader(MapFragment.class.getClassLoader());
        }
        super.onActivityCreated(savedInstanceState);
    }

    public void onAttach(Activity activity) {
        super.onAttach(activity);
        this.Pm.setActivity(activity);
    }

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        this.Pm.onCreate(savedInstanceState);
    }

    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        return this.Pm.onCreateView(inflater, container, savedInstanceState);
    }

    public void onDestroy() {
        this.Pm.onDestroy();
        super.onDestroy();
    }

    public void onDestroyView() {
        this.Pm.onDestroyView();
        super.onDestroyView();
    }

    public void onInflate(Activity activity, AttributeSet attrs, Bundle savedInstanceState) {
        super.onInflate(activity, attrs, savedInstanceState);
        this.Pm.setActivity(activity);
        Parcelable createFromAttributes = GoogleMapOptions.createFromAttributes(activity, attrs);
        Bundle bundle = new Bundle();
        bundle.putParcelable("MapOptions", createFromAttributes);
        this.Pm.onInflate(activity, bundle, savedInstanceState);
    }

    public void onLowMemory() {
        this.Pm.onLowMemory();
        super.onLowMemory();
    }

    public void onPause() {
        this.Pm.onPause();
        super.onPause();
    }

    public void onResume() {
        super.onResume();
        this.Pm.onResume();
    }

    public void onSaveInstanceState(Bundle outState) {
        if (outState != null) {
            outState.setClassLoader(MapFragment.class.getClassLoader());
        }
        super.onSaveInstanceState(outState);
        this.Pm.onSaveInstanceState(outState);
    }

    public void setArguments(Bundle args) {
        super.setArguments(args);
    }
}
