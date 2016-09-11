package com.google.android.gms.analytics.internal;

import android.os.Binder;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Parcel;
import android.os.RemoteException;
import java.util.List;
import java.util.Map;

public interface IAnalyticsService extends IInterface {

    public static abstract class Stub extends Binder implements IAnalyticsService {
        private static final String DESCRIPTOR = "com.google.android.gms.analytics.internal.IAnalyticsService";
        static final int TRANSACTION_clearHits = 2;
        static final int TRANSACTION_sendHit = 1;

        private static class Proxy implements IAnalyticsService {
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

            public void sendHit(Map wireParams, long hitTimeInMilliseconds, String path, List<Command> commands) throws RemoteException {
                Parcel _data = Parcel.obtain();
                Parcel _reply = Parcel.obtain();
                try {
                    _data.writeInterfaceToken(Stub.DESCRIPTOR);
                    _data.writeMap(wireParams);
                    _data.writeLong(hitTimeInMilliseconds);
                    _data.writeString(path);
                    _data.writeTypedList(commands);
                    this.mRemote.transact(Stub.TRANSACTION_sendHit, _data, _reply, 0);
                    _reply.readException();
                } finally {
                    _reply.recycle();
                    _data.recycle();
                }
            }

            public void clearHits() throws RemoteException {
                Parcel _data = Parcel.obtain();
                Parcel _reply = Parcel.obtain();
                try {
                    _data.writeInterfaceToken(Stub.DESCRIPTOR);
                    this.mRemote.transact(Stub.TRANSACTION_clearHits, _data, _reply, 0);
                    _reply.readException();
                } finally {
                    _reply.recycle();
                    _data.recycle();
                }
            }
        }

        public Stub() {
            attachInterface(this, DESCRIPTOR);
        }

        public static IAnalyticsService asInterface(IBinder obj) {
            if (obj == null) {
                return null;
            }
            IInterface iin = obj.queryLocalInterface(DESCRIPTOR);
            if (iin == null || !(iin instanceof IAnalyticsService)) {
                return new Proxy(obj);
            }
            return (IAnalyticsService) iin;
        }

        public IBinder asBinder() {
            return this;
        }

        public boolean onTransact(int code, Parcel data, Parcel reply, int flags) throws RemoteException {
            switch (code) {
                case TRANSACTION_sendHit /*1*/:
                    data.enforceInterface(DESCRIPTOR);
                    sendHit(data.readHashMap(getClass().getClassLoader()), data.readLong(), data.readString(), data.createTypedArrayList(Command.CREATOR));
                    reply.writeNoException();
                    return true;
                case TRANSACTION_clearHits /*2*/:
                    data.enforceInterface(DESCRIPTOR);
                    clearHits();
                    reply.writeNoException();
                    return true;
                case 1598968902:
                    reply.writeString(DESCRIPTOR);
                    return true;
                default:
                    return super.onTransact(code, data, reply, flags);
            }
        }
    }

    void clearHits() throws RemoteException;

    void sendHit(Map map, long j, String str, List<Command> list) throws RemoteException;
}
