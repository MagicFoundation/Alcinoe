package com.google.android.gms.wallet;

import android.accounts.Account;
import com.google.android.gms.auth.GoogleAuthUtil;

public final class WalletConstants {
    public static final String ACTION_ENABLE_WALLET_OPTIMIZATION = "com.google.android.gms.wallet.ENABLE_WALLET_OPTIMIZATION";
    public static final int ENVIRONMENT_PRODUCTION = 1;
    public static final int ENVIRONMENT_SANDBOX = 0;
    public static final int ENVIRONMENT_STRICT_SANDBOX = 2;
    public static final int ERROR_CODE_AUTHENTICATION_FAILURE = 411;
    public static final int ERROR_CODE_BUYER_ACCOUNT_ERROR = 409;
    public static final int ERROR_CODE_INVALID_PARAMETERS = 404;
    public static final int ERROR_CODE_INVALID_TRANSACTION = 410;
    public static final int ERROR_CODE_MERCHANT_ACCOUNT_ERROR = 405;
    public static final int ERROR_CODE_SERVICE_UNAVAILABLE = 402;
    public static final int ERROR_CODE_SPENDING_LIMIT_EXCEEDED = 406;
    public static final int ERROR_CODE_UNKNOWN = 413;
    public static final int ERROR_CODE_UNSUPPORTED_API_VERSION = 412;
    public static final String EXTRA_ERROR_CODE = "com.google.android.gms.wallet.EXTRA_ERROR_CODE";
    public static final String EXTRA_FULL_WALLET = "com.google.android.gms.wallet.EXTRA_FULL_WALLET";
    public static final String EXTRA_IS_USER_PREAUTHORIZED = "com.google.android.gm.wallet.EXTRA_IS_USER_PREAUTHORIZED";
    public static final String EXTRA_MASKED_WALLET = "com.google.android.gms.wallet.EXTRA_MASKED_WALLET";
    public static final int RESULT_ERROR = 1;
    public static final int THEME_HOLO_DARK = 0;
    public static final int THEME_HOLO_LIGHT = 1;
    public static final Account Zy;

    static {
        Zy = new Account("ACCOUNT_NO_WALLET", GoogleAuthUtil.GOOGLE_ACCOUNT_TYPE);
    }

    private WalletConstants() {
    }
}
