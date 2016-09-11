package com.google.android.gms.maps.internal;

import android.location.Location;
import android.os.Binder;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Parcel;
import android.os.RemoteException;
import android.support.v4.media.TransportMediator;
import android.support.v4.util.TimeUtils;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.maps.model.CameraPosition;
import com.google.android.gms.maps.model.CircleOptions;
import com.google.android.gms.maps.model.GroundOverlayOptions;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.PolygonOptions;
import com.google.android.gms.maps.model.PolylineOptions;
import com.google.android.gms.maps.model.TileOverlayOptions;
import com.google.android.gms.maps.model.internal.IPolylineDelegate;
import com.google.android.gms.maps.model.internal.b;
import com.google.android.gms.maps.model.internal.c;
import com.google.android.gms.maps.model.internal.d;
import com.google.android.gms.maps.model.internal.e;
import com.google.android.gms.maps.model.internal.f;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public interface IGoogleMapDelegate extends IInterface {

    public static abstract class a extends Binder implements IGoogleMapDelegate {

        private static class a implements IGoogleMapDelegate {
            private IBinder ky;

            a(IBinder iBinder) {
                this.ky = iBinder;
            }

            public b addCircle(CircleOptions options) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    if (options != null) {
                        obtain.writeInt(1);
                        options.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    this.ky.transact(35, obtain, obtain2, 0);
                    obtain2.readException();
                    b ao = com.google.android.gms.maps.model.internal.b.a.ao(obtain2.readStrongBinder());
                    return ao;
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public c addGroundOverlay(GroundOverlayOptions options) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    if (options != null) {
                        obtain.writeInt(1);
                        options.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    this.ky.transact(12, obtain, obtain2, 0);
                    obtain2.readException();
                    c ap = com.google.android.gms.maps.model.internal.c.a.ap(obtain2.readStrongBinder());
                    return ap;
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public d addMarker(MarkerOptions options) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    if (options != null) {
                        obtain.writeInt(1);
                        options.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    this.ky.transact(11, obtain, obtain2, 0);
                    obtain2.readException();
                    d aq = com.google.android.gms.maps.model.internal.d.a.aq(obtain2.readStrongBinder());
                    return aq;
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public e addPolygon(PolygonOptions options) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    if (options != null) {
                        obtain.writeInt(1);
                        options.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    this.ky.transact(10, obtain, obtain2, 0);
                    obtain2.readException();
                    e ar = com.google.android.gms.maps.model.internal.e.a.ar(obtain2.readStrongBinder());
                    return ar;
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public IPolylineDelegate addPolyline(PolylineOptions options) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    if (options != null) {
                        obtain.writeInt(1);
                        options.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    this.ky.transact(9, obtain, obtain2, 0);
                    obtain2.readException();
                    IPolylineDelegate as = com.google.android.gms.maps.model.internal.IPolylineDelegate.a.as(obtain2.readStrongBinder());
                    return as;
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public f addTileOverlay(TileOverlayOptions options) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    if (options != null) {
                        obtain.writeInt(1);
                        options.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    this.ky.transact(13, obtain, obtain2, 0);
                    obtain2.readException();
                    f at = com.google.android.gms.maps.model.internal.f.a.at(obtain2.readStrongBinder());
                    return at;
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void animateCamera(com.google.android.gms.dynamic.b update) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(update != null ? update.asBinder() : null);
                    this.ky.transact(5, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void animateCameraWithCallback(com.google.android.gms.dynamic.b update, b callback) throws RemoteException {
                IBinder iBinder = null;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(update != null ? update.asBinder() : null);
                    if (callback != null) {
                        iBinder = callback.asBinder();
                    }
                    obtain.writeStrongBinder(iBinder);
                    this.ky.transact(6, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void animateCameraWithDurationAndCallback(com.google.android.gms.dynamic.b update, int durationMs, b callback) throws RemoteException {
                IBinder iBinder = null;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(update != null ? update.asBinder() : null);
                    obtain.writeInt(durationMs);
                    if (callback != null) {
                        iBinder = callback.asBinder();
                    }
                    obtain.writeStrongBinder(iBinder);
                    this.ky.transact(7, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public IBinder asBinder() {
                return this.ky;
            }

            public void clear() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    this.ky.transact(14, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public CameraPosition getCameraPosition() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    this.ky.transact(1, obtain, obtain2, 0);
                    obtain2.readException();
                    CameraPosition createFromParcel = obtain2.readInt() != 0 ? CameraPosition.CREATOR.createFromParcel(obtain2) : null;
                    obtain2.recycle();
                    obtain.recycle();
                    return createFromParcel;
                } catch (Throwable th) {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public int getMapType() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    this.ky.transact(15, obtain, obtain2, 0);
                    obtain2.readException();
                    int readInt = obtain2.readInt();
                    return readInt;
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public float getMaxZoomLevel() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    this.ky.transact(2, obtain, obtain2, 0);
                    obtain2.readException();
                    float readFloat = obtain2.readFloat();
                    return readFloat;
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public float getMinZoomLevel() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    this.ky.transact(3, obtain, obtain2, 0);
                    obtain2.readException();
                    float readFloat = obtain2.readFloat();
                    return readFloat;
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public Location getMyLocation() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    this.ky.transact(23, obtain, obtain2, 0);
                    obtain2.readException();
                    Location location = obtain2.readInt() != 0 ? (Location) Location.CREATOR.createFromParcel(obtain2) : null;
                    obtain2.recycle();
                    obtain.recycle();
                    return location;
                } catch (Throwable th) {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public IProjectionDelegate getProjection() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    this.ky.transact(26, obtain, obtain2, 0);
                    obtain2.readException();
                    IProjectionDelegate ak = com.google.android.gms.maps.internal.IProjectionDelegate.a.ak(obtain2.readStrongBinder());
                    return ak;
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public com.google.android.gms.dynamic.b getTestingHelper() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    this.ky.transact(34, obtain, obtain2, 0);
                    obtain2.readException();
                    com.google.android.gms.dynamic.b G = com.google.android.gms.dynamic.b.a.G(obtain2.readStrongBinder());
                    return G;
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public IUiSettingsDelegate getUiSettings() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    this.ky.transact(25, obtain, obtain2, 0);
                    obtain2.readException();
                    IUiSettingsDelegate am = com.google.android.gms.maps.internal.IUiSettingsDelegate.a.am(obtain2.readStrongBinder());
                    return am;
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public boolean isBuildingsEnabled() throws RemoteException {
                boolean z = false;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    this.ky.transact(40, obtain, obtain2, 0);
                    obtain2.readException();
                    if (obtain2.readInt() != 0) {
                        z = true;
                    }
                    obtain2.recycle();
                    obtain.recycle();
                    return z;
                } catch (Throwable th) {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public boolean isIndoorEnabled() throws RemoteException {
                boolean z = false;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    this.ky.transact(19, obtain, obtain2, 0);
                    obtain2.readException();
                    if (obtain2.readInt() != 0) {
                        z = true;
                    }
                    obtain2.recycle();
                    obtain.recycle();
                    return z;
                } catch (Throwable th) {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public boolean isMyLocationEnabled() throws RemoteException {
                boolean z = false;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    this.ky.transact(21, obtain, obtain2, 0);
                    obtain2.readException();
                    if (obtain2.readInt() != 0) {
                        z = true;
                    }
                    obtain2.recycle();
                    obtain.recycle();
                    return z;
                } catch (Throwable th) {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public boolean isTrafficEnabled() throws RemoteException {
                boolean z = false;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    this.ky.transact(17, obtain, obtain2, 0);
                    obtain2.readException();
                    if (obtain2.readInt() != 0) {
                        z = true;
                    }
                    obtain2.recycle();
                    obtain.recycle();
                    return z;
                } catch (Throwable th) {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void moveCamera(com.google.android.gms.dynamic.b update) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(update != null ? update.asBinder() : null);
                    this.ky.transact(4, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setBuildingsEnabled(boolean enabled) throws RemoteException {
                int i = 0;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    if (enabled) {
                        i = 1;
                    }
                    obtain.writeInt(i);
                    this.ky.transact(41, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public boolean setIndoorEnabled(boolean enabled) throws RemoteException {
                boolean z = true;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeInt(enabled ? 1 : 0);
                    this.ky.transact(20, obtain, obtain2, 0);
                    obtain2.readException();
                    if (obtain2.readInt() == 0) {
                        z = false;
                    }
                    obtain2.recycle();
                    obtain.recycle();
                    return z;
                } catch (Throwable th) {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setInfoWindowAdapter(d adapter) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(adapter != null ? adapter.asBinder() : null);
                    this.ky.transact(33, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setLocationSource(ILocationSourceDelegate source) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(source != null ? source.asBinder() : null);
                    this.ky.transact(24, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setMapType(int type) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeInt(type);
                    this.ky.transact(16, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setMyLocationEnabled(boolean enabled) throws RemoteException {
                int i = 0;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    if (enabled) {
                        i = 1;
                    }
                    obtain.writeInt(i);
                    this.ky.transact(22, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setOnCameraChangeListener(e listener) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(listener != null ? listener.asBinder() : null);
                    this.ky.transact(27, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setOnInfoWindowClickListener(f listener) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(listener != null ? listener.asBinder() : null);
                    this.ky.transact(32, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setOnMapClickListener(h listener) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(listener != null ? listener.asBinder() : null);
                    this.ky.transact(28, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setOnMapLoadedCallback(i callback) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(callback != null ? callback.asBinder() : null);
                    this.ky.transact(42, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setOnMapLongClickListener(j listener) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(listener != null ? listener.asBinder() : null);
                    this.ky.transact(29, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setOnMarkerClickListener(k listener) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(listener != null ? listener.asBinder() : null);
                    this.ky.transact(30, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setOnMarkerDragListener(l listener) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(listener != null ? listener.asBinder() : null);
                    this.ky.transact(31, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setOnMyLocationButtonClickListener(m listener) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(listener != null ? listener.asBinder() : null);
                    this.ky.transact(37, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setOnMyLocationChangeListener(n listener) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(listener != null ? listener.asBinder() : null);
                    this.ky.transact(36, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setPadding(int left, int top, int right, int bottom) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeInt(left);
                    obtain.writeInt(top);
                    obtain.writeInt(right);
                    obtain.writeInt(bottom);
                    this.ky.transact(39, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setTrafficEnabled(boolean enabled) throws RemoteException {
                int i = 0;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    if (enabled) {
                        i = 1;
                    }
                    obtain.writeInt(i);
                    this.ky.transact(18, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void snapshot(o callback, com.google.android.gms.dynamic.b bitmap) throws RemoteException {
                IBinder iBinder = null;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    obtain.writeStrongBinder(callback != null ? callback.asBinder() : null);
                    if (bitmap != null) {
                        iBinder = bitmap.asBinder();
                    }
                    obtain.writeStrongBinder(iBinder);
                    this.ky.transact(38, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void stopAnimation() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    this.ky.transact(8, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }
        }

        public static IGoogleMapDelegate V(IBinder iBinder) {
            if (iBinder == null) {
                return null;
            }
            IInterface queryLocalInterface = iBinder.queryLocalInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
            return (queryLocalInterface == null || !(queryLocalInterface instanceof IGoogleMapDelegate)) ? new a(iBinder) : (IGoogleMapDelegate) queryLocalInterface;
        }

        public boolean onTransact(int code, Parcel data, Parcel reply, int flags) throws RemoteException {
            boolean z = false;
            IBinder iBinder = null;
            float maxZoomLevel;
            int mapType;
            boolean isTrafficEnabled;
            switch (code) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    CameraPosition cameraPosition = getCameraPosition();
                    reply.writeNoException();
                    if (cameraPosition != null) {
                        reply.writeInt(1);
                        cameraPosition.writeToParcel(reply, 1);
                        return true;
                    }
                    reply.writeInt(0);
                    return true;
                case DetectedActivity.ON_FOOT /*2*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    maxZoomLevel = getMaxZoomLevel();
                    reply.writeNoException();
                    reply.writeFloat(maxZoomLevel);
                    return true;
                case DetectedActivity.STILL /*3*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    maxZoomLevel = getMinZoomLevel();
                    reply.writeNoException();
                    reply.writeFloat(maxZoomLevel);
                    return true;
                case DetectedActivity.UNKNOWN /*4*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    moveCamera(com.google.android.gms.dynamic.b.a.G(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case DetectedActivity.TILTING /*5*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    animateCamera(com.google.android.gms.dynamic.b.a.G(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    animateCameraWithCallback(com.google.android.gms.dynamic.b.a.G(data.readStrongBinder()), com.google.android.gms.maps.internal.b.a.T(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case Error.AVS_DECLINE /*7*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    animateCameraWithDurationAndCallback(com.google.android.gms.dynamic.b.a.G(data.readStrongBinder()), data.readInt(), com.google.android.gms.maps.internal.b.a.T(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    stopAnimation();
                    reply.writeNoException();
                    return true;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    IPolylineDelegate addPolyline = addPolyline(data.readInt() != 0 ? PolylineOptions.CREATOR.createFromParcel(data) : null);
                    reply.writeNoException();
                    if (addPolyline != null) {
                        iBinder = addPolyline.asBinder();
                    }
                    reply.writeStrongBinder(iBinder);
                    return true;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    e addPolygon = addPolygon(data.readInt() != 0 ? PolygonOptions.CREATOR.createFromParcel(data) : null);
                    reply.writeNoException();
                    if (addPolygon != null) {
                        iBinder = addPolygon.asBinder();
                    }
                    reply.writeStrongBinder(iBinder);
                    return true;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    d addMarker = addMarker(data.readInt() != 0 ? MarkerOptions.CREATOR.createFromParcel(data) : null);
                    reply.writeNoException();
                    if (addMarker != null) {
                        iBinder = addMarker.asBinder();
                    }
                    reply.writeStrongBinder(iBinder);
                    return true;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    c addGroundOverlay = addGroundOverlay(data.readInt() != 0 ? GroundOverlayOptions.CREATOR.createFromParcel(data) : null);
                    reply.writeNoException();
                    if (addGroundOverlay != null) {
                        iBinder = addGroundOverlay.asBinder();
                    }
                    reply.writeStrongBinder(iBinder);
                    return true;
                case CommonStatusCodes.ERROR /*13*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    f addTileOverlay = addTileOverlay(data.readInt() != 0 ? TileOverlayOptions.CREATOR.createFromParcel(data) : null);
                    reply.writeNoException();
                    if (addTileOverlay != null) {
                        iBinder = addTileOverlay.asBinder();
                    }
                    reply.writeStrongBinder(iBinder);
                    return true;
                case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    clear();
                    reply.writeNoException();
                    return true;
                case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    mapType = getMapType();
                    reply.writeNoException();
                    reply.writeInt(mapType);
                    return true;
                case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    setMapType(data.readInt());
                    reply.writeNoException();
                    return true;
                case 17:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    isTrafficEnabled = isTrafficEnabled();
                    reply.writeNoException();
                    if (isTrafficEnabled) {
                        mapType = 1;
                    }
                    reply.writeInt(mapType);
                    return true;
                case 18:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    if (data.readInt() != 0) {
                        z = true;
                    }
                    setTrafficEnabled(z);
                    reply.writeNoException();
                    return true;
                case TimeUtils.HUNDRED_DAY_FIELD_LEN /*19*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    isTrafficEnabled = isIndoorEnabled();
                    reply.writeNoException();
                    if (isTrafficEnabled) {
                        mapType = 1;
                    }
                    reply.writeInt(mapType);
                    return true;
                case 20:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    isTrafficEnabled = setIndoorEnabled(data.readInt() != 0);
                    reply.writeNoException();
                    if (isTrafficEnabled) {
                        mapType = 1;
                    }
                    reply.writeInt(mapType);
                    return true;
                case 21:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    isTrafficEnabled = isMyLocationEnabled();
                    reply.writeNoException();
                    if (isTrafficEnabled) {
                        mapType = 1;
                    }
                    reply.writeInt(mapType);
                    return true;
                case 22:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    if (data.readInt() != 0) {
                        z = true;
                    }
                    setMyLocationEnabled(z);
                    reply.writeNoException();
                    return true;
                case 23:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    Location myLocation = getMyLocation();
                    reply.writeNoException();
                    if (myLocation != null) {
                        reply.writeInt(1);
                        myLocation.writeToParcel(reply, 1);
                        return true;
                    }
                    reply.writeInt(0);
                    return true;
                case 24:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    setLocationSource(com.google.android.gms.maps.internal.ILocationSourceDelegate.a.X(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case 25:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    IUiSettingsDelegate uiSettings = getUiSettings();
                    reply.writeNoException();
                    if (uiSettings != null) {
                        iBinder = uiSettings.asBinder();
                    }
                    reply.writeStrongBinder(iBinder);
                    return true;
                case 26:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    IProjectionDelegate projection = getProjection();
                    reply.writeNoException();
                    if (projection != null) {
                        iBinder = projection.asBinder();
                    }
                    reply.writeStrongBinder(iBinder);
                    return true;
                case 27:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    setOnCameraChangeListener(com.google.android.gms.maps.internal.e.a.aa(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case 28:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    setOnMapClickListener(com.google.android.gms.maps.internal.h.a.ad(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case 29:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    setOnMapLongClickListener(com.google.android.gms.maps.internal.j.a.af(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case 30:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    setOnMarkerClickListener(com.google.android.gms.maps.internal.k.a.ag(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case 31:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    setOnMarkerDragListener(com.google.android.gms.maps.internal.l.a.ah(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case TransportMediator.FLAG_KEY_MEDIA_STOP /*32*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    setOnInfoWindowClickListener(com.google.android.gms.maps.internal.f.a.ab(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case 33:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    setInfoWindowAdapter(com.google.android.gms.maps.internal.d.a.W(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case 34:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    com.google.android.gms.dynamic.b testingHelper = getTestingHelper();
                    reply.writeNoException();
                    if (testingHelper != null) {
                        iBinder = testingHelper.asBinder();
                    }
                    reply.writeStrongBinder(iBinder);
                    return true;
                case 35:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    b addCircle = addCircle(data.readInt() != 0 ? CircleOptions.CREATOR.createFromParcel(data) : null);
                    reply.writeNoException();
                    if (addCircle != null) {
                        iBinder = addCircle.asBinder();
                    }
                    reply.writeStrongBinder(iBinder);
                    return true;
                case 36:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    setOnMyLocationChangeListener(com.google.android.gms.maps.internal.n.a.aj(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case 37:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    setOnMyLocationButtonClickListener(com.google.android.gms.maps.internal.m.a.ai(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case 38:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    snapshot(com.google.android.gms.maps.internal.o.a.al(data.readStrongBinder()), com.google.android.gms.dynamic.b.a.G(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case 39:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    setPadding(data.readInt(), data.readInt(), data.readInt(), data.readInt());
                    reply.writeNoException();
                    return true;
                case 40:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    isTrafficEnabled = isBuildingsEnabled();
                    reply.writeNoException();
                    if (isTrafficEnabled) {
                        mapType = 1;
                    }
                    reply.writeInt(mapType);
                    return true;
                case 41:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    if (data.readInt() != 0) {
                        z = true;
                    }
                    setBuildingsEnabled(z);
                    reply.writeNoException();
                    return true;
                case 42:
                    data.enforceInterface("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    setOnMapLoadedCallback(com.google.android.gms.maps.internal.i.a.ae(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case 1598968902:
                    reply.writeString("com.google.android.gms.maps.internal.IGoogleMapDelegate");
                    return true;
                default:
                    return super.onTransact(code, data, reply, flags);
            }
        }
    }

    b addCircle(CircleOptions circleOptions) throws RemoteException;

    c addGroundOverlay(GroundOverlayOptions groundOverlayOptions) throws RemoteException;

    d addMarker(MarkerOptions markerOptions) throws RemoteException;

    e addPolygon(PolygonOptions polygonOptions) throws RemoteException;

    IPolylineDelegate addPolyline(PolylineOptions polylineOptions) throws RemoteException;

    f addTileOverlay(TileOverlayOptions tileOverlayOptions) throws RemoteException;

    void animateCamera(com.google.android.gms.dynamic.b bVar) throws RemoteException;

    void animateCameraWithCallback(com.google.android.gms.dynamic.b bVar, b bVar2) throws RemoteException;

    void animateCameraWithDurationAndCallback(com.google.android.gms.dynamic.b bVar, int i, b bVar2) throws RemoteException;

    void clear() throws RemoteException;

    CameraPosition getCameraPosition() throws RemoteException;

    int getMapType() throws RemoteException;

    float getMaxZoomLevel() throws RemoteException;

    float getMinZoomLevel() throws RemoteException;

    Location getMyLocation() throws RemoteException;

    IProjectionDelegate getProjection() throws RemoteException;

    com.google.android.gms.dynamic.b getTestingHelper() throws RemoteException;

    IUiSettingsDelegate getUiSettings() throws RemoteException;

    boolean isBuildingsEnabled() throws RemoteException;

    boolean isIndoorEnabled() throws RemoteException;

    boolean isMyLocationEnabled() throws RemoteException;

    boolean isTrafficEnabled() throws RemoteException;

    void moveCamera(com.google.android.gms.dynamic.b bVar) throws RemoteException;

    void setBuildingsEnabled(boolean z) throws RemoteException;

    boolean setIndoorEnabled(boolean z) throws RemoteException;

    void setInfoWindowAdapter(d dVar) throws RemoteException;

    void setLocationSource(ILocationSourceDelegate iLocationSourceDelegate) throws RemoteException;

    void setMapType(int i) throws RemoteException;

    void setMyLocationEnabled(boolean z) throws RemoteException;

    void setOnCameraChangeListener(e eVar) throws RemoteException;

    void setOnInfoWindowClickListener(f fVar) throws RemoteException;

    void setOnMapClickListener(h hVar) throws RemoteException;

    void setOnMapLoadedCallback(i iVar) throws RemoteException;

    void setOnMapLongClickListener(j jVar) throws RemoteException;

    void setOnMarkerClickListener(k kVar) throws RemoteException;

    void setOnMarkerDragListener(l lVar) throws RemoteException;

    void setOnMyLocationButtonClickListener(m mVar) throws RemoteException;

    void setOnMyLocationChangeListener(n nVar) throws RemoteException;

    void setPadding(int i, int i2, int i3, int i4) throws RemoteException;

    void setTrafficEnabled(boolean z) throws RemoteException;

    void snapshot(o oVar, com.google.android.gms.dynamic.b bVar) throws RemoteException;

    void stopAnimation() throws RemoteException;
}
