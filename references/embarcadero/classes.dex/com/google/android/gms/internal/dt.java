package com.google.android.gms.internal;

import android.os.Binder;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Parcel;
import android.os.RemoteException;
import android.support.v4.media.TransportMediator;
import com.google.android.gms.cast.ApplicationMetadata;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;

public interface dt extends IInterface {

    public static abstract class a extends Binder implements dt {
        public a() {
            attachInterface(this, "com.google.android.gms.cast.internal.ICastDeviceControllerListener");
        }

        public IBinder asBinder() {
            return this;
        }

        public boolean onTransact(int code, Parcel data, Parcel reply, int flags) throws RemoteException {
            boolean z = false;
            switch (code) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceControllerListener");
                    z(data.readInt());
                    return true;
                case DetectedActivity.ON_FOOT /*2*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceControllerListener");
                    ApplicationMetadata applicationMetadata = data.readInt() != 0 ? (ApplicationMetadata) ApplicationMetadata.CREATOR.createFromParcel(data) : null;
                    String readString = data.readString();
                    String readString2 = data.readString();
                    if (data.readInt() != 0) {
                        z = true;
                    }
                    a(applicationMetadata, readString, readString2, z);
                    return true;
                case DetectedActivity.STILL /*3*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceControllerListener");
                    A(data.readInt());
                    return true;
                case DetectedActivity.UNKNOWN /*4*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceControllerListener");
                    String readString3 = data.readString();
                    double readDouble = data.readDouble();
                    if (data.readInt() != 0) {
                        z = true;
                    }
                    b(readString3, readDouble, z);
                    return true;
                case DetectedActivity.TILTING /*5*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceControllerListener");
                    d(data.readString(), data.readString());
                    return true;
                case Participant.STATUS_UNRESPONSIVE /*6*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceControllerListener");
                    b(data.readString(), data.createByteArray());
                    return true;
                case Error.AVS_DECLINE /*7*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceControllerListener");
                    C(data.readInt());
                    return true;
                case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceControllerListener");
                    B(data.readInt());
                    return true;
                case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceControllerListener");
                    onApplicationDisconnected(data.readInt());
                    return true;
                case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceControllerListener");
                    a(data.readString(), data.readLong(), data.readInt());
                    return true;
                case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                    data.enforceInterface("com.google.android.gms.cast.internal.ICastDeviceControllerListener");
                    a(data.readString(), data.readLong());
                    return true;
                case 1598968902:
                    reply.writeString("com.google.android.gms.cast.internal.ICastDeviceControllerListener");
                    return true;
                default:
                    return super.onTransact(code, data, reply, flags);
            }
        }
    }

    void A(int i) throws RemoteException;

    void B(int i) throws RemoteException;

    void C(int i) throws RemoteException;

    void a(ApplicationMetadata applicationMetadata, String str, String str2, boolean z) throws RemoteException;

    void a(String str, long j) throws RemoteException;

    void a(String str, long j, int i) throws RemoteException;

    void b(String str, double d, boolean z) throws RemoteException;

    void b(String str, byte[] bArr) throws RemoteException;

    void d(String str, String str2) throws RemoteException;

    void onApplicationDisconnected(int i) throws RemoteException;

    void z(int i) throws RemoteException;
}
