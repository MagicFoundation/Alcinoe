package com.google.android.gms.maps;

import android.graphics.Bitmap;
import android.location.Location;
import android.os.RemoteException;
import android.view.View;
import com.google.android.gms.dynamic.b;
import com.google.android.gms.dynamic.c;
import com.google.android.gms.internal.er;
import com.google.android.gms.maps.LocationSource.OnLocationChangedListener;
import com.google.android.gms.maps.internal.IGoogleMapDelegate;
import com.google.android.gms.maps.internal.g;
import com.google.android.gms.maps.model.CameraPosition;
import com.google.android.gms.maps.model.Circle;
import com.google.android.gms.maps.model.CircleOptions;
import com.google.android.gms.maps.model.GroundOverlay;
import com.google.android.gms.maps.model.GroundOverlayOptions;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.Polygon;
import com.google.android.gms.maps.model.PolygonOptions;
import com.google.android.gms.maps.model.Polyline;
import com.google.android.gms.maps.model.PolylineOptions;
import com.google.android.gms.maps.model.RuntimeRemoteException;
import com.google.android.gms.maps.model.TileOverlay;
import com.google.android.gms.maps.model.TileOverlayOptions;
import com.google.android.gms.maps.model.internal.d;
import com.google.android.gms.maps.model.internal.f;

public final class GoogleMap {
    public static final int MAP_TYPE_HYBRID = 4;
    public static final int MAP_TYPE_NONE = 0;
    public static final int MAP_TYPE_NORMAL = 1;
    public static final int MAP_TYPE_SATELLITE = 2;
    public static final int MAP_TYPE_TERRAIN = 3;
    private final IGoogleMapDelegate OK;
    private UiSettings OL;

    public interface CancelableCallback {
        void onCancel();

        void onFinish();
    }

    public interface InfoWindowAdapter {
        View getInfoContents(Marker marker);

        View getInfoWindow(Marker marker);
    }

    public interface OnCameraChangeListener {
        void onCameraChange(CameraPosition cameraPosition);
    }

    public interface OnInfoWindowClickListener {
        void onInfoWindowClick(Marker marker);
    }

    public interface OnMapClickListener {
        void onMapClick(LatLng latLng);
    }

    public interface OnMapLoadedCallback {
        void onMapLoaded();
    }

    public interface OnMapLongClickListener {
        void onMapLongClick(LatLng latLng);
    }

    public interface OnMarkerClickListener {
        boolean onMarkerClick(Marker marker);
    }

    public interface OnMarkerDragListener {
        void onMarkerDrag(Marker marker);

        void onMarkerDragEnd(Marker marker);

        void onMarkerDragStart(Marker marker);
    }

    public interface OnMyLocationButtonClickListener {
        boolean onMyLocationButtonClick();
    }

    @Deprecated
    public interface OnMyLocationChangeListener {
        void onMyLocationChange(Location location);
    }

    public interface SnapshotReadyCallback {
        void onSnapshotReady(Bitmap bitmap);
    }

    /* renamed from: com.google.android.gms.maps.GoogleMap.10 */
    class AnonymousClass10 extends com.google.android.gms.maps.internal.f.a {
        final /* synthetic */ GoogleMap ON;
        final /* synthetic */ OnInfoWindowClickListener OY;

        AnonymousClass10(GoogleMap googleMap, OnInfoWindowClickListener onInfoWindowClickListener) {
            this.ON = googleMap;
            this.OY = onInfoWindowClickListener;
        }

        public void e(d dVar) {
            this.OY.onInfoWindowClick(new Marker(dVar));
        }
    }

    /* renamed from: com.google.android.gms.maps.GoogleMap.11 */
    class AnonymousClass11 extends com.google.android.gms.maps.internal.d.a {
        final /* synthetic */ GoogleMap ON;
        final /* synthetic */ InfoWindowAdapter OZ;

        AnonymousClass11(GoogleMap googleMap, InfoWindowAdapter infoWindowAdapter) {
            this.ON = googleMap;
            this.OZ = infoWindowAdapter;
        }

        public b f(d dVar) {
            return c.h(this.OZ.getInfoWindow(new Marker(dVar)));
        }

        public b g(d dVar) {
            return c.h(this.OZ.getInfoContents(new Marker(dVar)));
        }
    }

    /* renamed from: com.google.android.gms.maps.GoogleMap.12 */
    class AnonymousClass12 extends com.google.android.gms.maps.internal.n.a {
        final /* synthetic */ GoogleMap ON;
        final /* synthetic */ OnMyLocationChangeListener Pa;

        AnonymousClass12(GoogleMap googleMap, OnMyLocationChangeListener onMyLocationChangeListener) {
            this.ON = googleMap;
            this.Pa = onMyLocationChangeListener;
        }

        public void d(b bVar) {
            this.Pa.onMyLocationChange((Location) c.b(bVar));
        }
    }

    /* renamed from: com.google.android.gms.maps.GoogleMap.1 */
    class AnonymousClass1 extends com.google.android.gms.maps.internal.ILocationSourceDelegate.a {
        final /* synthetic */ LocationSource OM;
        final /* synthetic */ GoogleMap ON;

        /* renamed from: com.google.android.gms.maps.GoogleMap.1.1 */
        class AnonymousClass1 implements OnLocationChangedListener {
            final /* synthetic */ g OO;
            final /* synthetic */ AnonymousClass1 OP;

            AnonymousClass1(AnonymousClass1 anonymousClass1, g gVar) {
                this.OP = anonymousClass1;
                this.OO = gVar;
            }

            public void onLocationChanged(Location location) {
                try {
                    this.OO.g(c.h(location));
                } catch (RemoteException e) {
                    throw new RuntimeRemoteException(e);
                }
            }
        }

        AnonymousClass1(GoogleMap googleMap, LocationSource locationSource) {
            this.ON = googleMap;
            this.OM = locationSource;
        }

        public void activate(g listener) {
            this.OM.activate(new AnonymousClass1(this, listener));
        }

        public void deactivate() {
            this.OM.deactivate();
        }
    }

    /* renamed from: com.google.android.gms.maps.GoogleMap.2 */
    class AnonymousClass2 extends com.google.android.gms.maps.internal.m.a {
        final /* synthetic */ GoogleMap ON;
        final /* synthetic */ OnMyLocationButtonClickListener OQ;

        AnonymousClass2(GoogleMap googleMap, OnMyLocationButtonClickListener onMyLocationButtonClickListener) {
            this.ON = googleMap;
            this.OQ = onMyLocationButtonClickListener;
        }

        public boolean onMyLocationButtonClick() throws RemoteException {
            return this.OQ.onMyLocationButtonClick();
        }
    }

    /* renamed from: com.google.android.gms.maps.GoogleMap.3 */
    class AnonymousClass3 extends com.google.android.gms.maps.internal.i.a {
        final /* synthetic */ GoogleMap ON;
        final /* synthetic */ OnMapLoadedCallback OR;

        AnonymousClass3(GoogleMap googleMap, OnMapLoadedCallback onMapLoadedCallback) {
            this.ON = googleMap;
            this.OR = onMapLoadedCallback;
        }

        public void onMapLoaded() throws RemoteException {
            this.OR.onMapLoaded();
        }
    }

    /* renamed from: com.google.android.gms.maps.GoogleMap.4 */
    class AnonymousClass4 extends com.google.android.gms.maps.internal.o.a {
        final /* synthetic */ GoogleMap ON;
        final /* synthetic */ SnapshotReadyCallback OS;

        AnonymousClass4(GoogleMap googleMap, SnapshotReadyCallback snapshotReadyCallback) {
            this.ON = googleMap;
            this.OS = snapshotReadyCallback;
        }

        public void c(b bVar) throws RemoteException {
            this.OS.onSnapshotReady((Bitmap) c.b(bVar));
        }

        public void onSnapshotReady(Bitmap snapshot) throws RemoteException {
            this.OS.onSnapshotReady(snapshot);
        }
    }

    /* renamed from: com.google.android.gms.maps.GoogleMap.5 */
    class AnonymousClass5 extends com.google.android.gms.maps.internal.e.a {
        final /* synthetic */ GoogleMap ON;
        final /* synthetic */ OnCameraChangeListener OT;

        AnonymousClass5(GoogleMap googleMap, OnCameraChangeListener onCameraChangeListener) {
            this.ON = googleMap;
            this.OT = onCameraChangeListener;
        }

        public void onCameraChange(CameraPosition position) {
            this.OT.onCameraChange(position);
        }
    }

    /* renamed from: com.google.android.gms.maps.GoogleMap.6 */
    class AnonymousClass6 extends com.google.android.gms.maps.internal.h.a {
        final /* synthetic */ GoogleMap ON;
        final /* synthetic */ OnMapClickListener OU;

        AnonymousClass6(GoogleMap googleMap, OnMapClickListener onMapClickListener) {
            this.ON = googleMap;
            this.OU = onMapClickListener;
        }

        public void onMapClick(LatLng point) {
            this.OU.onMapClick(point);
        }
    }

    /* renamed from: com.google.android.gms.maps.GoogleMap.7 */
    class AnonymousClass7 extends com.google.android.gms.maps.internal.j.a {
        final /* synthetic */ GoogleMap ON;
        final /* synthetic */ OnMapLongClickListener OV;

        AnonymousClass7(GoogleMap googleMap, OnMapLongClickListener onMapLongClickListener) {
            this.ON = googleMap;
            this.OV = onMapLongClickListener;
        }

        public void onMapLongClick(LatLng point) {
            this.OV.onMapLongClick(point);
        }
    }

    /* renamed from: com.google.android.gms.maps.GoogleMap.8 */
    class AnonymousClass8 extends com.google.android.gms.maps.internal.k.a {
        final /* synthetic */ GoogleMap ON;
        final /* synthetic */ OnMarkerClickListener OW;

        AnonymousClass8(GoogleMap googleMap, OnMarkerClickListener onMarkerClickListener) {
            this.ON = googleMap;
            this.OW = onMarkerClickListener;
        }

        public boolean a(d dVar) {
            return this.OW.onMarkerClick(new Marker(dVar));
        }
    }

    /* renamed from: com.google.android.gms.maps.GoogleMap.9 */
    class AnonymousClass9 extends com.google.android.gms.maps.internal.l.a {
        final /* synthetic */ GoogleMap ON;
        final /* synthetic */ OnMarkerDragListener OX;

        AnonymousClass9(GoogleMap googleMap, OnMarkerDragListener onMarkerDragListener) {
            this.ON = googleMap;
            this.OX = onMarkerDragListener;
        }

        public void b(d dVar) {
            this.OX.onMarkerDragStart(new Marker(dVar));
        }

        public void c(d dVar) {
            this.OX.onMarkerDragEnd(new Marker(dVar));
        }

        public void d(d dVar) {
            this.OX.onMarkerDrag(new Marker(dVar));
        }
    }

    private static final class a extends com.google.android.gms.maps.internal.b.a {
        private final CancelableCallback Pb;

        a(CancelableCallback cancelableCallback) {
            this.Pb = cancelableCallback;
        }

        public void onCancel() {
            this.Pb.onCancel();
        }

        public void onFinish() {
            this.Pb.onFinish();
        }
    }

    protected GoogleMap(IGoogleMapDelegate map) {
        this.OK = (IGoogleMapDelegate) er.f(map);
    }

    public final Circle addCircle(CircleOptions options) {
        try {
            return new Circle(this.OK.addCircle(options));
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final GroundOverlay addGroundOverlay(GroundOverlayOptions options) {
        try {
            com.google.android.gms.maps.model.internal.c addGroundOverlay = this.OK.addGroundOverlay(options);
            return addGroundOverlay != null ? new GroundOverlay(addGroundOverlay) : null;
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final Marker addMarker(MarkerOptions options) {
        try {
            d addMarker = this.OK.addMarker(options);
            return addMarker != null ? new Marker(addMarker) : null;
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final Polygon addPolygon(PolygonOptions options) {
        try {
            return new Polygon(this.OK.addPolygon(options));
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final Polyline addPolyline(PolylineOptions options) {
        try {
            return new Polyline(this.OK.addPolyline(options));
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final TileOverlay addTileOverlay(TileOverlayOptions options) {
        try {
            f addTileOverlay = this.OK.addTileOverlay(options);
            return addTileOverlay != null ? new TileOverlay(addTileOverlay) : null;
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final void animateCamera(CameraUpdate update) {
        try {
            this.OK.animateCamera(update.gK());
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final void animateCamera(CameraUpdate update, int durationMs, CancelableCallback callback) {
        try {
            this.OK.animateCameraWithDurationAndCallback(update.gK(), durationMs, callback == null ? null : new a(callback));
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final void animateCamera(CameraUpdate update, CancelableCallback callback) {
        try {
            this.OK.animateCameraWithCallback(update.gK(), callback == null ? null : new a(callback));
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final void clear() {
        try {
            this.OK.clear();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    IGoogleMapDelegate gM() {
        return this.OK;
    }

    public final CameraPosition getCameraPosition() {
        try {
            return this.OK.getCameraPosition();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final int getMapType() {
        try {
            return this.OK.getMapType();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final float getMaxZoomLevel() {
        try {
            return this.OK.getMaxZoomLevel();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final float getMinZoomLevel() {
        try {
            return this.OK.getMinZoomLevel();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    @Deprecated
    public final Location getMyLocation() {
        try {
            return this.OK.getMyLocation();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final Projection getProjection() {
        try {
            return new Projection(this.OK.getProjection());
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final UiSettings getUiSettings() {
        try {
            if (this.OL == null) {
                this.OL = new UiSettings(this.OK.getUiSettings());
            }
            return this.OL;
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final boolean isBuildingsEnabled() {
        try {
            return this.OK.isBuildingsEnabled();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final boolean isIndoorEnabled() {
        try {
            return this.OK.isIndoorEnabled();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final boolean isMyLocationEnabled() {
        try {
            return this.OK.isMyLocationEnabled();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final boolean isTrafficEnabled() {
        try {
            return this.OK.isTrafficEnabled();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final void moveCamera(CameraUpdate update) {
        try {
            this.OK.moveCamera(update.gK());
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final void setBuildingsEnabled(boolean enabled) {
        try {
            this.OK.setBuildingsEnabled(enabled);
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final boolean setIndoorEnabled(boolean enabled) {
        try {
            return this.OK.setIndoorEnabled(enabled);
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final void setInfoWindowAdapter(InfoWindowAdapter adapter) {
        if (adapter == null) {
            try {
                this.OK.setInfoWindowAdapter(null);
                return;
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }
        this.OK.setInfoWindowAdapter(new AnonymousClass11(this, adapter));
    }

    public final void setLocationSource(LocationSource source) {
        if (source == null) {
            try {
                this.OK.setLocationSource(null);
                return;
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }
        this.OK.setLocationSource(new AnonymousClass1(this, source));
    }

    public final void setMapType(int type) {
        try {
            this.OK.setMapType(type);
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final void setMyLocationEnabled(boolean enabled) {
        try {
            this.OK.setMyLocationEnabled(enabled);
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final void setOnCameraChangeListener(OnCameraChangeListener listener) {
        if (listener == null) {
            try {
                this.OK.setOnCameraChangeListener(null);
                return;
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }
        this.OK.setOnCameraChangeListener(new AnonymousClass5(this, listener));
    }

    public final void setOnInfoWindowClickListener(OnInfoWindowClickListener listener) {
        if (listener == null) {
            try {
                this.OK.setOnInfoWindowClickListener(null);
                return;
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }
        this.OK.setOnInfoWindowClickListener(new AnonymousClass10(this, listener));
    }

    public final void setOnMapClickListener(OnMapClickListener listener) {
        if (listener == null) {
            try {
                this.OK.setOnMapClickListener(null);
                return;
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }
        this.OK.setOnMapClickListener(new AnonymousClass6(this, listener));
    }

    public void setOnMapLoadedCallback(OnMapLoadedCallback callback) {
        if (callback == null) {
            try {
                this.OK.setOnMapLoadedCallback(null);
                return;
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }
        this.OK.setOnMapLoadedCallback(new AnonymousClass3(this, callback));
    }

    public final void setOnMapLongClickListener(OnMapLongClickListener listener) {
        if (listener == null) {
            try {
                this.OK.setOnMapLongClickListener(null);
                return;
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }
        this.OK.setOnMapLongClickListener(new AnonymousClass7(this, listener));
    }

    public final void setOnMarkerClickListener(OnMarkerClickListener listener) {
        if (listener == null) {
            try {
                this.OK.setOnMarkerClickListener(null);
                return;
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }
        this.OK.setOnMarkerClickListener(new AnonymousClass8(this, listener));
    }

    public final void setOnMarkerDragListener(OnMarkerDragListener listener) {
        if (listener == null) {
            try {
                this.OK.setOnMarkerDragListener(null);
                return;
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }
        this.OK.setOnMarkerDragListener(new AnonymousClass9(this, listener));
    }

    public final void setOnMyLocationButtonClickListener(OnMyLocationButtonClickListener listener) {
        if (listener == null) {
            try {
                this.OK.setOnMyLocationButtonClickListener(null);
                return;
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }
        this.OK.setOnMyLocationButtonClickListener(new AnonymousClass2(this, listener));
    }

    @Deprecated
    public final void setOnMyLocationChangeListener(OnMyLocationChangeListener listener) {
        if (listener == null) {
            try {
                this.OK.setOnMyLocationChangeListener(null);
                return;
            } catch (RemoteException e) {
                throw new RuntimeRemoteException(e);
            }
        }
        this.OK.setOnMyLocationChangeListener(new AnonymousClass12(this, listener));
    }

    public final void setPadding(int left, int top, int right, int bottom) {
        try {
            this.OK.setPadding(left, top, right, bottom);
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final void setTrafficEnabled(boolean enabled) {
        try {
            this.OK.setTrafficEnabled(enabled);
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final void snapshot(SnapshotReadyCallback callback) {
        snapshot(callback, null);
    }

    public final void snapshot(SnapshotReadyCallback callback, Bitmap bitmap) {
        try {
            this.OK.snapshot(new AnonymousClass4(this, callback), (c) (bitmap != null ? c.h(bitmap) : null));
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }

    public final void stopAnimation() {
        try {
            this.OK.stopAnimation();
        } catch (RemoteException e) {
            throw new RuntimeRemoteException(e);
        }
    }
}
