package com.google.android.vending.licensing;

import android.os.Binder;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Parcel;
import android.os.RemoteException;

public interface ILicenseResultListener extends IInterface {

    public static abstract class Stub extends Binder implements ILicenseResultListener {
        private static final String DESCRIPTOR = "com.android.vending.licensing.ILicenseResultListener";
        static final int TRANSACTION_verifyLicense = 1;

        private static class Proxy implements ILicenseResultListener {
            private IBinder mRemote;

            Proxy(IBinder remote) {
                this.mRemote = remote;
            }

            public IBinder asBinder() {
                return this.mRemote;
            }

            public String getInterfaceDescriptor() {
                return Stub.DESCRIPTOR;
            }

            public void verifyLicense(int responseCode, String signedData, String signature) throws RemoteException {
                Parcel _data = Parcel.obtain();
                try {
                    _data.writeInterfaceToken(Stub.DESCRIPTOR);
                    _data.writeInt(responseCode);
                    _data.writeString(signedData);
                    _data.writeString(signature);
                    this.mRemote.transact(Stub.TRANSACTION_verifyLicense, _data, null, Stub.TRANSACTION_verifyLicense);
                } finally {
                    _data.recycle();
                }
            }
        }

        public Stub() {
            attachInterface(this, DESCRIPTOR);
        }

        public static ILicenseResultListener asInterface(IBinder obj) {
            if (obj == null) {
                return null;
            }
            IInterface iin = obj.queryLocalInterface(DESCRIPTOR);
            if (iin == null || !(iin instanceof ILicenseResultListener)) {
                return new Proxy(obj);
            }
            return (ILicenseResultListener) iin;
        }

        public IBinder asBinder() {
            return this;
        }

        public boolean onTransact(int code, Parcel data, Parcel reply, int flags) throws RemoteException {
            switch (code) {
                case TRANSACTION_verifyLicense /*1*/:
                    data.enforceInterface(DESCRIPTOR);
                    verifyLicense(data.readInt(), data.readString(), data.readString());
                    return true;
                case 1598968902:
                    reply.writeString(DESCRIPTOR);
                    return true;
                default:
                    return super.onTransact(code, data, reply, flags);
            }
        }
    }

    void verifyLicense(int i, String str, String str2) throws RemoteException;
}
