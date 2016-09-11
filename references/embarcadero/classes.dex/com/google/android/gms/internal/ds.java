package com.google.android.gms.internal;

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

public interface ds extends IInterface {

    public static abstract class a extends Binder implements ds {

        private static class a implements ds {
            private IBinder ky;

            a(IBinder iBinder) {
                this.ky = iBinder;
            }

            public void R(String str) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.cast.internal.ICastDeviceController");
                    obtain.writeString(str);
                    this.ky.transact(5, obtain, null, 1);
                } finally {
                    obtain.recycle();
                }
            }

            public void S(String str) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.cast.internal.ICastDeviceController");
                    obtain.writeString(str);
                    this.ky.transact(11, obtain, null, 1);
                } finally {
                    obtain.recycle();
                }
            }

            public void T(String str) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.cast.internal.ICastDeviceController");
                    obtain.writeString(str);
                    this.ky.transact(12, obtain, null, 1);
                } finally {
                    obtain.recycle();
                }
            }

            public void a(double d, double d2, boolean z) throws RemoteException {
                int i = 1;
                Parcel obtain = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.cast.internal.ICastDeviceController");
                    obtain.writeDouble(d);
                    obtain.writeDouble(d2);
                    if (!z) {
                        i = 0;
                    }
                    obtain.writeInt(i);
                    this.ky.transact(7, obtain, null, 1);
                } finally {
                    obtain.recycle();
                }
            }

            public void a(String str, String str2, long j) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.cast.internal.ICastDeviceController");
                    obtain.writeString(str);
                    obtain.writeString(str2);
                    obtain.writeLong(j);
                    this.ky.transact(9, obtain, null, 1);
                } finally {
                    obtain.recycle();
                }
            }

            public void a(String str, byte[] bArr, long j) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.cast.internal.ICastDeviceController");
                    obtain.writeString(str);
                    obtain.writeByteArray(bArr);
                    obtain.writeLong(j);
                    this.ky.transact(10, obtain, null, 1);
                } finally {
                    obtain.recycle();
                }
            }

            public void a(boolean z, double d, boolean z2) throws RemoteException {
                int i = 1;
                Parcel obtain = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.cast.internal.ICastDeviceController");
                    obtain.writeInt(z ? 1 : 0);
                    obtain.writeDouble(d);
                    if (!z2) {
                        i = 0;
                    }
                    obtain.writeInt(i);
                    this.ky.transact(8, obtain, null, 1);
                } finally {
                    obtain.recycle();
                }
            }

            public IBinder asBinder() {
                return this.ky;
            }

            public void cZ() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.cast.internal.ICastDeviceController");
                    this.ky.transact(6, obtain, null, 1);
                } finally {
                    obtain.recycle();
                }
            }

            public void df() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.cast.internal.ICastDeviceController");
                    this.ky.transact(4, obtain, null, 1);
                } finally {
                    obtain.recycle();
                }
            }

            public void disconnect() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.cast.internal.ICastDeviceController");
                    this.ky.transact(1, obtain, null, 1);
                } finally {
                    obtain.recycle();
                }
            }

            public void e(String str, String str2) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.cast.internal.ICastDeviceController");
                    obtain.writeString(str);
                    obtain.writeString(str2);
                    this.ky.transact(3, obtain, null, 1);
                } finally {
                    obtain.recycle();
                }
            }

            public void e(String str, boolean z) throws RemoteException {
                int i = 1;
                Parcel obtain = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.cast.internal.ICastDeviceController");
                    obtain.writeString(str);
                    if (!z) {
                        i = 0;
                    }
                    obtain.writeInt(i);
                    this.ky.transact(2, obtain, null, 1);
                } finally {
                    obtain.recycle();
                }
            }
        }

        public static ds w(IBinder iBinder) {
            if (iBinder == null) {
                return null;
            }
            IInterface queryLocalInterface = iBinder.queryLocalInterface("com.google.android.gms.cast.internal.ICastDeviceController");
            return (queryLocalInterface == null || !(queryLocalInterface instanceof ds)) ? new a(iBinder) : (ds) queryLocalInterface;
        }

        public boolean onTransact(int code, Parcel data, Parcel reply, int flags) throws RemoteException {
            boolean z = false;
            switch (code) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceController");
                    disconnect();
                    return true;
                case DetectedActivity.ON_FOOT /*2*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceController");
                    String readString = data.readString();
                    if (data.readInt() != 0) {
                        z = true;
                    }
                    e(readString, z);
                    return true;
                case DetectedActivity.STILL /*3*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceController");
                    e(data.readString(), data.readString());
                    return true;
                case DetectedActivity.UNKNOWN /*4*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceController");
                    df();
                    return true;
                case DetectedActivity.TILTING /*5*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceController");
                    R(data.readString());
                    return true;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceController");
                    cZ();
                    return true;
                case Error.AVS_DECLINE /*7*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceController");
                    a(data.readDouble(), data.readDouble(), data.readInt() != 0);
                    return true;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceController");
                    boolean z2 = data.readInt() != 0;
                    double readDouble = data.readDouble();
                    if (data.readInt() != 0) {
                        z = true;
                    }
                    a(z2, readDouble, z);
                    return true;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceController");
                    a(data.readString(), data.readString(), data.readLong());
                    return true;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceController");
                    a(data.readString(), data.createByteArray(), data.readLong());
                    return true;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceController");
                    S(data.readString());
                    return true;
                case CommonStatusCodes.DATE_INVALID /*12*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceController");
                    T(data.readString());
                    return true;
                case 1598968902:
                    reply.writeString("com.google.android.gms.cast.internal.ICastDeviceController");
                    return true;
                default:
                    return super.onTransact(code, data, reply, flags);
            }
        }
    }

    void R(String str) throws RemoteException;

    void S(String str) throws RemoteException;

    void T(String str) throws RemoteException;

    void a(double d, double d2, boolean z) throws RemoteException;

    void a(String str, String str2, long j) throws RemoteException;

    void a(String str, byte[] bArr, long j) throws RemoteException;

    void a(boolean z, double d, boolean z2) throws RemoteException;

    void cZ() throws RemoteException;

    void df() throws RemoteException;

    void disconnect() throws RemoteException;

    void e(String str, String str2) throws RemoteException;

    void e(String str, boolean z) throws RemoteException;
}
