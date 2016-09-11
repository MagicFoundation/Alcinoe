package com.google.android.gms.internal;

import android.os.Binder;
import android.os.Bundle;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Parcel;
import android.os.RemoteException;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public interface bw extends IInterface {

    public static abstract class a extends Binder implements bw {

        private static class a implements bw {
            private IBinder ky;

            a(IBinder iBinder) {
                this.ky = iBinder;
            }

            public IBinder asBinder() {
                return this.ky;
            }

            public void onCreate(Bundle savedInstanceState) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    if (savedInstanceState != null) {
                        obtain.writeInt(1);
                        savedInstanceState.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    this.ky.transact(1, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void onDestroy() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    this.ky.transact(8, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void onPause() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    this.ky.transact(5, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void onRestart() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    this.ky.transact(2, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void onResume() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    this.ky.transact(4, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void onSaveInstanceState(Bundle outState) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    if (outState != null) {
                        obtain.writeInt(1);
                        outState.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    this.ky.transact(6, obtain, obtain2, 0);
                    obtain2.readException();
                    if (obtain2.readInt() != 0) {
                        outState.readFromParcel(obtain2);
                    }
                    obtain2.recycle();
                    obtain.recycle();
                } catch (Throwable th) {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void onStart() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    this.ky.transact(3, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void onStop() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    this.ky.transact(7, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }
        }

        public a() {
            attachInterface(this, "com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
        }

        public static bw m(IBinder iBinder) {
            if (iBinder == null) {
                return null;
            }
            IInterface queryLocalInterface = iBinder.queryLocalInterface("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
            return (queryLocalInterface == null || !(queryLocalInterface instanceof bw)) ? new a(iBinder) : (bw) queryLocalInterface;
        }

        public IBinder asBinder() {
            return this;
        }

        public boolean onTransact(int code, Parcel data, Parcel reply, int flags) throws RemoteException {
            Bundle bundle = null;
            switch (code) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    data.enforceInterface("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    if (data.readInt() != 0) {
                        bundle = (Bundle) Bundle.CREATOR.createFromParcel(data);
                    }
                    onCreate(bundle);
                    reply.writeNoException();
                    return true;
                case DetectedActivity.ON_FOOT /*2*/:
                    data.enforceInterface("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    onRestart();
                    reply.writeNoException();
                    return true;
                case DetectedActivity.STILL /*3*/:
                    data.enforceInterface("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    onStart();
                    reply.writeNoException();
                    return true;
                case DetectedActivity.UNKNOWN /*4*/:
                    data.enforceInterface("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    onResume();
                    reply.writeNoException();
                    return true;
                case DetectedActivity.TILTING /*5*/:
                    data.enforceInterface("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    onPause();
                    reply.writeNoException();
                    return true;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    data.enforceInterface("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    if (data.readInt() != 0) {
                        bundle = (Bundle) Bundle.CREATOR.createFromParcel(data);
                    }
                    onSaveInstanceState(bundle);
                    reply.writeNoException();
                    if (bundle != null) {
                        reply.writeInt(1);
                        bundle.writeToParcel(reply, 1);
                    } else {
                        reply.writeInt(0);
                    }
                    return true;
                case Error.AVS_DECLINE /*7*/:
                    data.enforceInterface("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    onStop();
                    reply.writeNoException();
                    return true;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    data.enforceInterface("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    onDestroy();
                    reply.writeNoException();
                    return true;
                case 1598968902:
                    reply.writeString("com.google.android.gms.ads.internal.overlay.client.IAdOverlay");
                    return true;
                default:
                    return super.onTransact(code, data, reply, flags);
            }
        }
    }

    void onCreate(Bundle bundle) throws RemoteException;

    void onDestroy() throws RemoteException;

    void onPause() throws RemoteException;

    void onRestart() throws RemoteException;

    void onResume() throws RemoteException;

    void onSaveInstanceState(Bundle bundle) throws RemoteException;

    void onStart() throws RemoteException;

    void onStop() throws RemoteException;
}
