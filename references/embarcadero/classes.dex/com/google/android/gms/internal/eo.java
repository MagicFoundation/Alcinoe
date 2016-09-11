package com.google.android.gms.internal;

import android.os.Binder;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Parcel;
import android.os.RemoteException;
import com.google.android.gms.dynamic.b;
import com.google.android.vending.licensing.APKExpansionPolicy;

public interface eo extends IInterface {

    public static abstract class a extends Binder implements eo {

        private static class a implements eo {
            private IBinder ky;

            a(IBinder iBinder) {
                this.ky = iBinder;
            }

            public b a(b bVar, int i, int i2) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.common.internal.ISignInButtonCreator");
                    obtain.writeStrongBinder(bVar != null ? bVar.asBinder() : null);
                    obtain.writeInt(i);
                    obtain.writeInt(i2);
                    this.ky.transact(1, obtain, obtain2, 0);
                    obtain2.readException();
                    b G = com.google.android.gms.dynamic.b.a.G(obtain2.readStrongBinder());
                    return G;
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public IBinder asBinder() {
                return this.ky;
            }
        }

        public static eo A(IBinder iBinder) {
            if (iBinder == null) {
                return null;
            }
            IInterface queryLocalInterface = iBinder.queryLocalInterface("com.google.android.gms.common.internal.ISignInButtonCreator");
            return (queryLocalInterface == null || !(queryLocalInterface instanceof eo)) ? new a(iBinder) : (eo) queryLocalInterface;
        }

        public boolean onTransact(int code, Parcel data, Parcel reply, int flags) throws RemoteException {
            switch (code) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    data.enforceInterface("com.google.android.gms.common.internal.ISignInButtonCreator");
                    b a = a(com.google.android.gms.dynamic.b.a.G(data.readStrongBinder()), data.readInt(), data.readInt());
                    reply.writeNoException();
                    reply.writeStrongBinder(a != null ? a.asBinder() : null);
                    return true;
                case 1598968902:
                    reply.writeString("com.google.android.gms.common.internal.ISignInButtonCreator");
                    return true;
                default:
                    return super.onTransact(code, data, reply, flags);
            }
        }
    }

    b a(b bVar, int i, int i2) throws RemoteException;
}
