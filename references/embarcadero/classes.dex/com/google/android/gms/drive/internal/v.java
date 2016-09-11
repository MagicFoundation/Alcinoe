package com.google.android.gms.drive.internal;

import android.os.Binder;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Parcel;
import android.os.RemoteException;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public interface v extends IInterface {

    public static abstract class a extends Binder implements v {

        private static class a implements v {
            private IBinder ky;

            a(IBinder iBinder) {
                this.ky = iBinder;
            }

            public void a(OnContentsResponse onContentsResponse) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    if (onContentsResponse != null) {
                        obtain.writeInt(1);
                        onContentsResponse.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    this.ky.transact(5, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void a(OnDownloadProgressResponse onDownloadProgressResponse) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    if (onDownloadProgressResponse != null) {
                        obtain.writeInt(1);
                        onDownloadProgressResponse.writeToParcel(obtain, 0);
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

            public void a(OnDriveIdResponse onDriveIdResponse) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    if (onDriveIdResponse != null) {
                        obtain.writeInt(1);
                        onDriveIdResponse.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    this.ky.transact(3, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void a(OnListEntriesResponse onListEntriesResponse) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    if (onListEntriesResponse != null) {
                        obtain.writeInt(1);
                        onListEntriesResponse.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    this.ky.transact(2, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void a(OnListParentsResponse onListParentsResponse) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    if (onListParentsResponse != null) {
                        obtain.writeInt(1);
                        onListParentsResponse.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    this.ky.transact(8, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void a(OnMetadataResponse onMetadataResponse) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    if (onMetadataResponse != null) {
                        obtain.writeInt(1);
                        onMetadataResponse.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    this.ky.transact(4, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public IBinder asBinder() {
                return this.ky;
            }

            public void l(Status status) throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    if (status != null) {
                        obtain.writeInt(1);
                        status.writeToParcel(obtain, 0);
                    } else {
                        obtain.writeInt(0);
                    }
                    this.ky.transact(6, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }

            public void onSuccess() throws RemoteException {
                Parcel obtain = Parcel.obtain();
                Parcel obtain2 = Parcel.obtain();
                try {
                    obtain.writeInterfaceToken("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    this.ky.transact(7, obtain, obtain2, 0);
                    obtain2.readException();
                } finally {
                    obtain2.recycle();
                    obtain.recycle();
                }
            }
        }

        public a() {
            attachInterface(this, "com.google.android.gms.drive.internal.IDriveServiceCallbacks");
        }

        public static v E(IBinder iBinder) {
            if (iBinder == null) {
                return null;
            }
            IInterface queryLocalInterface = iBinder.queryLocalInterface("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
            return (queryLocalInterface == null || !(queryLocalInterface instanceof v)) ? new a(iBinder) : (v) queryLocalInterface;
        }

        public IBinder asBinder() {
            return this;
        }

        public boolean onTransact(int code, Parcel data, Parcel reply, int flags) throws RemoteException {
            OnListParentsResponse onListParentsResponse = null;
            switch (code) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    OnDownloadProgressResponse onDownloadProgressResponse;
                    data.enforceInterface("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    if (data.readInt() != 0) {
                        onDownloadProgressResponse = (OnDownloadProgressResponse) OnDownloadProgressResponse.CREATOR.createFromParcel(data);
                    }
                    a(onDownloadProgressResponse);
                    reply.writeNoException();
                    return true;
                case DetectedActivity.ON_FOOT /*2*/:
                    OnListEntriesResponse onListEntriesResponse;
                    data.enforceInterface("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    if (data.readInt() != 0) {
                        onListEntriesResponse = (OnListEntriesResponse) OnListEntriesResponse.CREATOR.createFromParcel(data);
                    }
                    a(onListEntriesResponse);
                    reply.writeNoException();
                    return true;
                case DetectedActivity.STILL /*3*/:
                    OnDriveIdResponse onDriveIdResponse;
                    data.enforceInterface("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    if (data.readInt() != 0) {
                        onDriveIdResponse = (OnDriveIdResponse) OnDriveIdResponse.CREATOR.createFromParcel(data);
                    }
                    a(onDriveIdResponse);
                    reply.writeNoException();
                    return true;
                case DetectedActivity.UNKNOWN /*4*/:
                    OnMetadataResponse onMetadataResponse;
                    data.enforceInterface("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    if (data.readInt() != 0) {
                        onMetadataResponse = (OnMetadataResponse) OnMetadataResponse.CREATOR.createFromParcel(data);
                    }
                    a(onMetadataResponse);
                    reply.writeNoException();
                    return true;
                case DetectedActivity.TILTING /*5*/:
                    OnContentsResponse onContentsResponse;
                    data.enforceInterface("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    if (data.readInt() != 0) {
                        onContentsResponse = (OnContentsResponse) OnContentsResponse.CREATOR.createFromParcel(data);
                    }
                    a(onContentsResponse);
                    reply.writeNoException();
                    return true;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    Status createFromParcel;
                    data.enforceInterface("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    if (data.readInt() != 0) {
                        createFromParcel = Status.CREATOR.createFromParcel(data);
                    }
                    l(createFromParcel);
                    reply.writeNoException();
                    return true;
                case Error.AVS_DECLINE /*7*/:
                    data.enforceInterface("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    onSuccess();
                    reply.writeNoException();
                    return true;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    data.enforceInterface("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    if (data.readInt() != 0) {
                        onListParentsResponse = (OnListParentsResponse) OnListParentsResponse.CREATOR.createFromParcel(data);
                    }
                    a(onListParentsResponse);
                    reply.writeNoException();
                    return true;
                case 1598968902:
                    reply.writeString("com.google.android.gms.drive.internal.IDriveServiceCallbacks");
                    return true;
                default:
                    return super.onTransact(code, data, reply, flags);
            }
        }
    }

    void a(OnContentsResponse onContentsResponse) throws RemoteException;

    void a(OnDownloadProgressResponse onDownloadProgressResponse) throws RemoteException;

    void a(OnDriveIdResponse onDriveIdResponse) throws RemoteException;

    void a(OnListEntriesResponse onListEntriesResponse) throws RemoteException;

    void a(OnListParentsResponse onListParentsResponse) throws RemoteException;

    void a(OnMetadataResponse onMetadataResponse) throws RemoteException;

    void l(Status status) throws RemoteException;

    void onSuccess() throws RemoteException;
}
