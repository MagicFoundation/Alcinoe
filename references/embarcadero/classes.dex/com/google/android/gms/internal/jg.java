package com.google.android.gms.internal;

import android.accounts.Account;
import android.app.Activity;
import android.app.PendingIntent;
import android.content.Intent;
import android.os.Bundle;
import android.os.IBinder;
import android.os.IInterface;
import android.os.Looper;
import android.os.RemoteException;
import android.text.TextUtils;
import android.util.Log;
import com.google.android.gms.auth.GoogleAuthUtil;
import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.GooglePlayServicesClient;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.internal.eh.c;
import com.google.android.gms.internal.eh.e;
import com.google.android.gms.internal.eh.g;
import com.google.android.gms.wallet.FullWallet;
import com.google.android.gms.wallet.FullWalletRequest;
import com.google.android.gms.wallet.MaskedWallet;
import com.google.android.gms.wallet.MaskedWalletRequest;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest;
import com.google.android.gms.wallet.WalletConstants;

public class jg extends eh<je> {
    private final int Zw;
    private final int mTheme;
    private final Activity nd;
    private final String vi;

    final class a extends com.google.android.gms.internal.jf.a {
        private final int Bq;
        final /* synthetic */ jg Zz;

        public a(jg jgVar, int i) {
            this.Zz = jgVar;
            this.Bq = i;
        }

        public void a(int i, FullWallet fullWallet, Bundle bundle) {
            PendingIntent pendingIntent = null;
            if (bundle != null) {
                pendingIntent = (PendingIntent) bundle.getParcelable("com.google.android.gms.wallet.EXTRA_PENDING_INTENT");
            }
            ConnectionResult connectionResult = new ConnectionResult(i, pendingIntent);
            if (connectionResult.hasResolution()) {
                try {
                    connectionResult.startResolutionForResult(this.Zz.nd, this.Bq);
                    return;
                } catch (Throwable e) {
                    Log.w("WalletClientImpl", "Exception starting pending intent", e);
                    return;
                }
            }
            int i2;
            Intent intent = new Intent();
            if (connectionResult.isSuccess()) {
                i2 = -1;
                intent.putExtra(WalletConstants.EXTRA_FULL_WALLET, fullWallet);
            } else {
                i2 = i == 408 ? 0 : 1;
                intent.putExtra(WalletConstants.EXTRA_ERROR_CODE, i);
            }
            PendingIntent createPendingResult = this.Zz.nd.createPendingResult(this.Bq, intent, 1073741824);
            if (createPendingResult == null) {
                Log.w("WalletClientImpl", "Null pending result returned for onFullWalletLoaded");
                return;
            }
            try {
                createPendingResult.send(i2);
            } catch (Throwable e2) {
                Log.w("WalletClientImpl", "Exception setting pending result", e2);
            }
        }

        public void a(int i, MaskedWallet maskedWallet, Bundle bundle) {
            PendingIntent pendingIntent = null;
            if (bundle != null) {
                pendingIntent = (PendingIntent) bundle.getParcelable("com.google.android.gms.wallet.EXTRA_PENDING_INTENT");
            }
            ConnectionResult connectionResult = new ConnectionResult(i, pendingIntent);
            if (connectionResult.hasResolution()) {
                try {
                    connectionResult.startResolutionForResult(this.Zz.nd, this.Bq);
                    return;
                } catch (Throwable e) {
                    Log.w("WalletClientImpl", "Exception starting pending intent", e);
                    return;
                }
            }
            int i2;
            Intent intent = new Intent();
            if (connectionResult.isSuccess()) {
                i2 = -1;
                intent.putExtra(WalletConstants.EXTRA_MASKED_WALLET, maskedWallet);
            } else {
                i2 = i == 408 ? 0 : 1;
                intent.putExtra(WalletConstants.EXTRA_ERROR_CODE, i);
            }
            PendingIntent createPendingResult = this.Zz.nd.createPendingResult(this.Bq, intent, 1073741824);
            if (createPendingResult == null) {
                Log.w("WalletClientImpl", "Null pending result returned for onMaskedWalletLoaded");
                return;
            }
            try {
                createPendingResult.send(i2);
            } catch (Throwable e2) {
                Log.w("WalletClientImpl", "Exception setting pending result", e2);
            }
        }

        public void a(int i, boolean z, Bundle bundle) {
            Intent intent = new Intent();
            intent.putExtra(WalletConstants.EXTRA_IS_USER_PREAUTHORIZED, z);
            PendingIntent createPendingResult = this.Zz.nd.createPendingResult(this.Bq, intent, 1073741824);
            if (createPendingResult == null) {
                Log.w("WalletClientImpl", "Null pending result returned for onPreAuthorizationDetermined");
                return;
            }
            try {
                createPendingResult.send(-1);
            } catch (Throwable e) {
                Log.w("WalletClientImpl", "Exception setting pending result", e);
            }
        }

        public void e(int i, Bundle bundle) {
            er.b((Object) bundle, (Object) "Bundle should not be null");
            ConnectionResult connectionResult = new ConnectionResult(i, (PendingIntent) bundle.getParcelable("com.google.android.gms.wallet.EXTRA_PENDING_INTENT"));
            if (connectionResult.hasResolution()) {
                try {
                    connectionResult.startResolutionForResult(this.Zz.nd, this.Bq);
                    return;
                } catch (Throwable e) {
                    Log.w("WalletClientImpl", "Exception starting pending intent", e);
                    return;
                }
            }
            Log.e("WalletClientImpl", "Create Wallet Objects confirmation UI will not be shown connection result: " + connectionResult);
            Intent intent = new Intent();
            intent.putExtra(WalletConstants.EXTRA_ERROR_CODE, WalletConstants.ERROR_CODE_UNKNOWN);
            PendingIntent createPendingResult = this.Zz.nd.createPendingResult(this.Bq, intent, 1073741824);
            if (createPendingResult == null) {
                Log.w("WalletClientImpl", "Null pending result returned for onWalletObjectsCreated");
                return;
            }
            try {
                createPendingResult.send(1);
            } catch (Throwable e2) {
                Log.w("WalletClientImpl", "Exception setting pending result", e2);
            }
        }
    }

    public jg(Activity activity, Looper looper, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener, int i, String str, int i2) {
        super(activity, looper, connectionCallbacks, onConnectionFailedListener, new String[0]);
        this.nd = activity;
        this.Zw = i;
        this.vi = str;
        this.mTheme = i2;
    }

    @Deprecated
    public jg(Activity activity, GooglePlayServicesClient.ConnectionCallbacks connectionCallbacks, GooglePlayServicesClient.OnConnectionFailedListener onConnectionFailedListener, int i, String str, int i2) {
        this(activity, activity.getMainLooper(), new c(connectionCallbacks), new g(onConnectionFailedListener), i, str, i2);
    }

    private Bundle kx() {
        Bundle bundle = new Bundle();
        bundle.putInt("com.google.android.gms.wallet.EXTRA_ENVIRONMENT", this.Zw);
        bundle.putString("androidPackageName", this.nd.getPackageName());
        if (!TextUtils.isEmpty(this.vi)) {
            bundle.putParcelable("com.google.android.gms.wallet.EXTRA_BUYER_ACCOUNT", new Account(this.vi, GoogleAuthUtil.GOOGLE_ACCOUNT_TYPE));
        }
        bundle.putInt("com.google.android.gms.wallet.EXTRA_THEME", this.mTheme);
        return bundle;
    }

    protected void a(en enVar, e eVar) throws RemoteException {
        enVar.a(eVar, GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_VERSION_CODE);
    }

    protected je aE(IBinder iBinder) {
        return com.google.android.gms.internal.je.a.aC(iBinder);
    }

    protected String aF() {
        return "com.google.android.gms.wallet.service.BIND";
    }

    protected String aG() {
        return "com.google.android.gms.wallet.internal.IOwService";
    }

    public void changeMaskedWallet(String googleTransactionId, String merchantTransactionId, int requestCode) {
        Bundle kx = kx();
        Object aVar = new a(this, requestCode);
        try {
            ((je) eb()).a(googleTransactionId, merchantTransactionId, kx, aVar);
        } catch (Throwable e) {
            Log.e("WalletClientImpl", "RemoteException changing masked wallet", e);
            aVar.a(8, null, null);
        }
    }

    public void checkForPreAuthorization(int requestCode) {
        Bundle kx = kx();
        jf aVar = new a(this, requestCode);
        try {
            ((je) eb()).a(kx, aVar);
        } catch (Throwable e) {
            Log.e("WalletClientImpl", "RemoteException during checkForPreAuthorization", e);
            aVar.a(8, false, null);
        }
    }

    public void loadFullWallet(FullWalletRequest request, int requestCode) {
        jf aVar = new a(this, requestCode);
        try {
            ((je) eb()).a(request, kx(), aVar);
        } catch (Throwable e) {
            Log.e("WalletClientImpl", "RemoteException getting full wallet", e);
            aVar.a(8, null, null);
        }
    }

    public void loadMaskedWallet(MaskedWalletRequest request, int requestCode) {
        Bundle kx = kx();
        jf aVar = new a(this, requestCode);
        try {
            ((je) eb()).a(request, kx, aVar);
        } catch (Throwable e) {
            Log.e("WalletClientImpl", "RemoteException getting masked wallet", e);
            aVar.a(8, null, null);
        }
    }

    public void notifyTransactionStatus(NotifyTransactionStatusRequest request) {
        try {
            ((je) eb()).a(request, kx());
        } catch (RemoteException e) {
        }
    }

    protected /* synthetic */ IInterface p(IBinder iBinder) {
        return aE(iBinder);
    }
}
