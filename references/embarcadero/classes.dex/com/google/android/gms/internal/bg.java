package com.google.android.gms.internal;

import android.os.Binder;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Parcel;
import android.os.RemoteException;
import com.google.android.gms.dynamic.b;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public interface bg extends IInterface {

    public static abstract class a extends Binder implements bg {

        private static class a implements bg {
            private IBinder ky;

            a(IBinder iBinder) {
                this.ky = iBinder;
            }

            public void a(b bVar, ab abVar, z zVar, String str, bh bhVar) throws RemoteException {
                IBinder iBinder = null;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
                    obtain.writeStrongBinder(bVar != null ? bVar.asBinder() : null);
                    if (abVar != null) {
                        obtain.writeInt(1);
                        abVar.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    if (zVar != null) {
                        obtain.writeInt(1);
                        zVar.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    obtain.writeString(str);
                    if (bhVar != null) {
                        iBinder = bhVar.asBinder();
                    }
                    obtain.writeStrongBinder(iBinder);
                    this.ky.transact(1, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void a(b bVar, ab abVar, z zVar, String str, String str2, bh bhVar) throws RemoteException {
                IBinder iBinder = null;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
                    obtain.writeStrongBinder(bVar != null ? bVar.asBinder() : null);
                    if (abVar != null) {
                        obtain.writeInt(1);
                        abVar.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    if (zVar != null) {
                        obtain.writeInt(1);
                        zVar.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    obtain.writeString(str);
                    obtain.writeString(str2);
                    if (bhVar != null) {
                        iBinder = bhVar.asBinder();
                    }
                    obtain.writeStrongBinder(iBinder);
                    this.ky.transact(6, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void a(b bVar, z zVar, String str, bh bhVar) throws RemoteException {
                IBinder iBinder = null;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
                    obtain.writeStrongBinder(bVar != null ? bVar.asBinder() : null);
                    if (zVar != null) {
                        obtain.writeInt(1);
                        zVar.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    obtain.writeString(str);
                    if (bhVar != null) {
                        iBinder = bhVar.asBinder();
                    }
                    obtain.writeStrongBinder(iBinder);
                    this.ky.transact(3, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void a(b bVar, z zVar, String str, String str2, bh bhVar) throws RemoteException {
                IBinder iBinder = null;
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
                    obtain.writeStrongBinder(bVar != null ? bVar.asBinder() : null);
                    if (zVar != null) {
                        obtain.writeInt(1);
                        zVar.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    obtain.writeString(str);
                    obtain.writeString(str2);
                    if (bhVar != null) {
                        iBinder = bhVar.asBinder();
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

            public void destroy() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
                    this.ky.transact(5, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public b getView() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
                    this.ky.transact(2, obtain, obtain2, 0);
                    obtain2.readException();
                    b G = com.google.android.gms.dynamic.b.a.G(obtain2.readStrongBinder());
                    return G;
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void showInterstitial() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
                    this.ky.transact(4, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }
        }

        public a() {
            attachInterface(this, "com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
        }

        public static bg j(IBinder iBinder) {
            if (iBinder == null) {
                return null;
            }
            IInterface queryLocalInterface = iBinder.queryLocalInterface("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
            return (queryLocalInterface == null || !(queryLocalInterface instanceof bg)) ? new a(iBinder) : (bg) queryLocalInterface;
        }

        public IBinder asBinder() {
            return this;
        }

        public boolean onTransact(int code, Parcel data, Parcel reply, int flags) throws RemoteException {
            z zVar = null;
            b view;
            switch (code) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    data.enforceInterface("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
                    a(com.google.android.gms.dynamic.b.a.G(data.readStrongBinder()), data.readInt() != 0 ? ab.CREATOR.b(data) : null, data.readInt() != 0 ? z.CREATOR.a(data) : null, data.readString(), com.google.android.gms.internal.bh.a.k(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case DetectedActivity.ON_FOOT /*2*/:
                    IBinder asBinder;
                    data.enforceInterface("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
                    view = getView();
                    reply.writeNoException();
                    if (view != null) {
                        asBinder = view.asBinder();
                    }
                    reply.writeStrongBinder(asBinder);
                    return true;
                case DetectedActivity.STILL /*3*/:
                    data.enforceInterface("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
                    view = com.google.android.gms.dynamic.b.a.G(data.readStrongBinder());
                    if (data.readInt() != 0) {
                        zVar = z.CREATOR.a(data);
                    }
                    a(view, zVar, data.readString(), com.google.android.gms.internal.bh.a.k(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case DetectedActivity.UNKNOWN /*4*/:
                    data.enforceInterface("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
                    showInterstitial();
                    reply.writeNoException();
                    return true;
                case DetectedActivity.TILTING /*5*/:
                    data.enforceInterface("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
                    destroy();
                    reply.writeNoException();
                    return true;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    data.enforceInterface("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
                    a(com.google.android.gms.dynamic.b.a.G(data.readStrongBinder()), data.readInt() != 0 ? ab.CREATOR.b(data) : null, data.readInt() != 0 ? z.CREATOR.a(data) : null, data.readString(), data.readString(), com.google.android.gms.internal.bh.a.k(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case Error.AVS_DECLINE /*7*/:
                    data.enforceInterface("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
                    a(com.google.android.gms.dynamic.b.a.G(data.readStrongBinder()), data.readInt() != 0 ? z.CREATOR.a(data) : null, data.readString(), data.readString(), com.google.android.gms.internal.bh.a.k(data.readStrongBinder()));
                    reply.writeNoException();
                    return true;
                case 1598968902:
                    reply.writeString("com.google.android.gms.ads.internal.mediation.client.IMediationAdapter");
                    return true;
                default:
                    return super.onTransact(code, data, reply, flags);
            }
        }
    }

    void a(b bVar, ab abVar, z zVar, String str, bh bhVar) throws RemoteException;

    void a(b bVar, ab abVar, z zVar, String str, String str2, bh bhVar) throws RemoteException;

    void a(b bVar, z zVar, String str, bh bhVar) throws RemoteException;

    void a(b bVar, z zVar, String str, String str2, bh bhVar) throws RemoteException;

    void destroy() throws RemoteException;

    b getView() throws RemoteException;

    void showInterstitial() throws RemoteException;
}
