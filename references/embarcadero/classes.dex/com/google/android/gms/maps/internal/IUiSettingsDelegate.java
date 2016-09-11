package com.google.android.gms.maps.internal;

import android.os.Binder;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Parcel;
import android.os.RemoteException;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public interface IUiSettingsDelegate extends IInterface {

    public static abstract class a extends Binder implements IUiSettingsDelegate {

        private static class a implements IUiSettingsDelegate {
            private IBinder ky;

            a(IBinder iBinder) {
                this.ky = iBinder;
            }

            public IBinder asBinder() {
                return this.ky;
            }

            public boolean isCompassEnabled() throws RemoteException {
                boolean z = false;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    this.ky.transact(10, obtain, obtain2, 0);
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

            public boolean isMyLocationButtonEnabled() throws RemoteException {
                boolean z = false;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    this.ky.transact(11, obtain, obtain2, 0);
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

            public boolean isRotateGesturesEnabled() throws RemoteException {
                boolean z = false;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    this.ky.transact(15, obtain, obtain2, 0);
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

            public boolean isScrollGesturesEnabled() throws RemoteException {
                boolean z = false;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    this.ky.transact(12, obtain, obtain2, 0);
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

            public boolean isTiltGesturesEnabled() throws RemoteException {
                boolean z = false;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    this.ky.transact(14, obtain, obtain2, 0);
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

            public boolean isZoomControlsEnabled() throws RemoteException {
                boolean z = false;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    this.ky.transact(9, obtain, obtain2, 0);
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

            public boolean isZoomGesturesEnabled() throws RemoteException {
                boolean z = false;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    this.ky.transact(13, obtain, obtain2, 0);
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

            public void setAllGesturesEnabled(boolean enabled) throws RemoteException {
                int i = 0;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (enabled) {
                        i = 1;
                    }
                    obtain.writeInt(i);
                    this.ky.transact(8, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setCompassEnabled(boolean enabled) throws RemoteException {
                int i = 0;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (enabled) {
                        i = 1;
                    }
                    obtain.writeInt(i);
                    this.ky.transact(2, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setMyLocationButtonEnabled(boolean enabled) throws RemoteException {
                int i = 0;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (enabled) {
                        i = 1;
                    }
                    obtain.writeInt(i);
                    this.ky.transact(3, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setRotateGesturesEnabled(boolean enabled) throws RemoteException {
                int i = 0;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (enabled) {
                        i = 1;
                    }
                    obtain.writeInt(i);
                    this.ky.transact(7, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setScrollGesturesEnabled(boolean enabled) throws RemoteException {
                int i = 0;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (enabled) {
                        i = 1;
                    }
                    obtain.writeInt(i);
                    this.ky.transact(4, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setTiltGesturesEnabled(boolean enabled) throws RemoteException {
                int i = 0;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (enabled) {
                        i = 1;
                    }
                    obtain.writeInt(i);
                    this.ky.transact(6, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setZoomControlsEnabled(boolean enabled) throws RemoteException {
                int i = 1;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (!enabled) {
                        i = 0;
                    }
                    obtain.writeInt(i);
                    this.ky.transact(1, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void setZoomGesturesEnabled(boolean enabled) throws RemoteException {
                int i = 0;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (enabled) {
                        i = 1;
                    }
                    obtain.writeInt(i);
                    this.ky.transact(5, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }
        }

        public static IUiSettingsDelegate am(IBinder iBinder) {
            if (iBinder == null) {
                return null;
            }
            IInterface queryLocalInterface = iBinder.queryLocalInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
            return (queryLocalInterface == null || !(queryLocalInterface instanceof IUiSettingsDelegate)) ? new a(iBinder) : (IUiSettingsDelegate) queryLocalInterface;
        }

        public boolean onTransact(int code, Parcel data, Parcel reply, int flags) throws RemoteException {
            int i = 0;
            boolean z;
            boolean isZoomControlsEnabled;
            switch (code) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (data.readInt() != 0) {
                        z = true;
                    }
                    setZoomControlsEnabled(z);
                    reply.writeNoException();
                    return true;
                case DetectedActivity.ON_FOOT /*2*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (data.readInt() != 0) {
                        z = true;
                    }
                    setCompassEnabled(z);
                    reply.writeNoException();
                    return true;
                case DetectedActivity.STILL /*3*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (data.readInt() != 0) {
                        z = true;
                    }
                    setMyLocationButtonEnabled(z);
                    reply.writeNoException();
                    return true;
                case DetectedActivity.UNKNOWN /*4*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (data.readInt() != 0) {
                        z = true;
                    }
                    setScrollGesturesEnabled(z);
                    reply.writeNoException();
                    return true;
                case DetectedActivity.TILTING /*5*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (data.readInt() != 0) {
                        z = true;
                    }
                    setZoomGesturesEnabled(z);
                    reply.writeNoException();
                    return true;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (data.readInt() != 0) {
                        z = true;
                    }
                    setTiltGesturesEnabled(z);
                    reply.writeNoException();
                    return true;
                case Error.AVS_DECLINE /*7*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (data.readInt() != 0) {
                        z = true;
                    }
                    setRotateGesturesEnabled(z);
                    reply.writeNoException();
                    return true;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    if (data.readInt() != 0) {
                        z = true;
                    }
                    setAllGesturesEnabled(z);
                    reply.writeNoException();
                    return true;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    isZoomControlsEnabled = isZoomControlsEnabled();
                    reply.writeNoException();
                    if (isZoomControlsEnabled) {
                        i = 1;
                    }
                    reply.writeInt(i);
                    return true;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    isZoomControlsEnabled = isCompassEnabled();
                    reply.writeNoException();
                    if (isZoomControlsEnabled) {
                        i = 1;
                    }
                    reply.writeInt(i);
                    return true;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    isZoomControlsEnabled = isMyLocationButtonEnabled();
                    reply.writeNoException();
                    if (isZoomControlsEnabled) {
                        i = 1;
                    }
                    reply.writeInt(i);
                    return true;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    isZoomControlsEnabled = isScrollGesturesEnabled();
                    reply.writeNoException();
                    if (isZoomControlsEnabled) {
                        i = 1;
                    }
                    reply.writeInt(i);
                    return true;
                case CommonStatusCodes.ERROR /*13*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    isZoomControlsEnabled = isZoomGesturesEnabled();
                    reply.writeNoException();
                    if (isZoomControlsEnabled) {
                        i = 1;
                    }
                    reply.writeInt(i);
                    return true;
                case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    isZoomControlsEnabled = isTiltGesturesEnabled();
                    reply.writeNoException();
                    if (isZoomControlsEnabled) {
                        i = 1;
                    }
                    reply.writeInt(i);
                    return true;
                case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                    data.enforceInterface("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    isZoomControlsEnabled = isRotateGesturesEnabled();
                    reply.writeNoException();
                    if (isZoomControlsEnabled) {
                        i = 1;
                    }
                    reply.writeInt(i);
                    return true;
                case 1598968902:
                    reply.writeString("com.google.android.gms.maps.internal.IUiSettingsDelegate");
                    return true;
                default:
                    return super.onTransact(code, data, reply, flags);
            }
        }
    }

    boolean isCompassEnabled() throws RemoteException;

    boolean isMyLocationButtonEnabled() throws RemoteException;

    boolean isRotateGesturesEnabled() throws RemoteException;

    boolean isScrollGesturesEnabled() throws RemoteException;

    boolean isTiltGesturesEnabled() throws RemoteException;

    boolean isZoomControlsEnabled() throws RemoteException;

    boolean isZoomGesturesEnabled() throws RemoteException;

    void setAllGesturesEnabled(boolean z) throws RemoteException;

    void setCompassEnabled(boolean z) throws RemoteException;

    void setMyLocationButtonEnabled(boolean z) throws RemoteException;

    void setRotateGesturesEnabled(boolean z) throws RemoteException;

    void setScrollGesturesEnabled(boolean z) throws RemoteException;

    void setTiltGesturesEnabled(boolean z) throws RemoteException;

    void setZoomControlsEnabled(boolean z) throws RemoteException;

    void setZoomGesturesEnabled(boolean z) throws RemoteException;
}
