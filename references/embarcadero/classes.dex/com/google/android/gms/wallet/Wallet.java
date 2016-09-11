package com.google.android.gms.wallet;

import android.app.Activity;
import android.content.Context;
import android.os.Looper;
import com.google.android.gms.common.api.Api;
import com.google.android.gms.common.api.Api.b;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.GoogleApiClient.ApiOptions;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.common.api.Result;
import com.google.android.gms.common.api.Scope;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.internal.ee;
import com.google.android.gms.internal.er;
import com.google.android.gms.internal.jg;

public final class Wallet {
    public static final Api API;
    static final b<jg> va;

    public static final class WalletOptions implements ApiOptions {
        public final int environment;
        public final int theme;

        public static final class Builder {
            private int Zw;
            private int mTheme;

            public Builder() {
                this.Zw = 0;
                this.mTheme = 0;
            }

            public WalletOptions build() {
                return new WalletOptions();
            }

            public Builder setEnvironment(int environment) {
                if (environment == 0 || environment == 2 || environment == 1) {
                    this.Zw = environment;
                    return this;
                }
                throw new IllegalArgumentException(String.format("Invalid environment value %d", new Object[]{Integer.valueOf(environment)}));
            }

            public Builder setTheme(int theme) {
                if (theme == 0 || theme == 1) {
                    this.mTheme = theme;
                    return this;
                }
                throw new IllegalArgumentException(String.format("Invalid theme value %d", new Object[]{Integer.valueOf(theme)}));
            }
        }

        private WalletOptions() {
            this(new Builder());
        }

        private WalletOptions(Builder builder) {
            this.environment = builder.Zw;
            this.theme = builder.mTheme;
        }
    }

    private static abstract class a extends com.google.android.gms.common.api.a.a<Status, jg> {
        public a() {
            super(Wallet.va);
        }

        public /* synthetic */ Result d(Status status) {
            return f(status);
        }

        public Status f(Status status) {
            return status;
        }
    }

    /* renamed from: com.google.android.gms.wallet.Wallet.2 */
    static class AnonymousClass2 extends a {
        final /* synthetic */ int Kx;

        AnonymousClass2(int i) {
            this.Kx = i;
        }

        protected void a(jg jgVar) {
            jgVar.checkForPreAuthorization(this.Kx);
            a(Status.zQ);
        }
    }

    /* renamed from: com.google.android.gms.wallet.Wallet.3 */
    static class AnonymousClass3 extends a {
        final /* synthetic */ int Kx;
        final /* synthetic */ MaskedWalletRequest Zr;

        AnonymousClass3(MaskedWalletRequest maskedWalletRequest, int i) {
            this.Zr = maskedWalletRequest;
            this.Kx = i;
        }

        protected void a(jg jgVar) {
            jgVar.loadMaskedWallet(this.Zr, this.Kx);
            a(Status.zQ);
        }
    }

    /* renamed from: com.google.android.gms.wallet.Wallet.4 */
    static class AnonymousClass4 extends a {
        final /* synthetic */ int Kx;
        final /* synthetic */ FullWalletRequest Zs;

        AnonymousClass4(FullWalletRequest fullWalletRequest, int i) {
            this.Zs = fullWalletRequest;
            this.Kx = i;
        }

        protected void a(jg jgVar) {
            jgVar.loadFullWallet(this.Zs, this.Kx);
            a(Status.zQ);
        }
    }

    /* renamed from: com.google.android.gms.wallet.Wallet.5 */
    static class AnonymousClass5 extends a {
        final /* synthetic */ int Kx;
        final /* synthetic */ String Zt;
        final /* synthetic */ String Zu;

        AnonymousClass5(String str, String str2, int i) {
            this.Zt = str;
            this.Zu = str2;
            this.Kx = i;
        }

        protected void a(jg jgVar) {
            jgVar.changeMaskedWallet(this.Zt, this.Zu, this.Kx);
            a(Status.zQ);
        }
    }

    /* renamed from: com.google.android.gms.wallet.Wallet.6 */
    static class AnonymousClass6 extends a {
        final /* synthetic */ NotifyTransactionStatusRequest Zv;

        AnonymousClass6(NotifyTransactionStatusRequest notifyTransactionStatusRequest) {
            this.Zv = notifyTransactionStatusRequest;
        }

        protected void a(jg jgVar) {
            jgVar.notifyTransactionStatus(this.Zv);
            a(Status.zQ);
        }
    }

    static {
        va = new b<jg>() {
            public /* synthetic */ com.google.android.gms.common.api.Api.a b(Context context, Looper looper, ee eeVar, ApiOptions apiOptions, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
                return i(context, looper, eeVar, apiOptions, connectionCallbacks, onConnectionFailedListener);
            }

            public int getPriority() {
                return Integer.MAX_VALUE;
            }

            public jg i(Context context, Looper looper, ee eeVar, ApiOptions apiOptions, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
                er.b(context instanceof Activity, (Object) "An Activity must be used for Wallet APIs");
                Activity activity = (Activity) context;
                boolean z = apiOptions == null || (apiOptions instanceof WalletOptions);
                er.b(z, (Object) "WalletOptions must be used for Wallet APIs");
                WalletOptions walletOptions = apiOptions != null ? (WalletOptions) apiOptions : new WalletOptions();
                return new jg(activity, looper, connectionCallbacks, onConnectionFailedListener, walletOptions.environment, eeVar.getAccountName(), walletOptions.theme);
            }
        };
        API = new Api(va, new Scope[0]);
    }

    private Wallet() {
    }

    public static void changeMaskedWallet(GoogleApiClient googleApiClient, String googleTransactionId, String merchantTransactionId, int requestCode) {
        googleApiClient.a(new AnonymousClass5(googleTransactionId, merchantTransactionId, requestCode));
    }

    public static void checkForPreAuthorization(GoogleApiClient googleApiClient, int requestCode) {
        googleApiClient.a(new AnonymousClass2(requestCode));
    }

    public static void loadFullWallet(GoogleApiClient googleApiClient, FullWalletRequest request, int requestCode) {
        googleApiClient.a(new AnonymousClass4(request, requestCode));
    }

    public static void loadMaskedWallet(GoogleApiClient googleApiClient, MaskedWalletRequest request, int requestCode) {
        googleApiClient.a(new AnonymousClass3(request, requestCode));
    }

    public static void notifyTransactionStatus(GoogleApiClient googleApiClient, NotifyTransactionStatusRequest request) {
        googleApiClient.a(new AnonymousClass6(request));
    }
}
