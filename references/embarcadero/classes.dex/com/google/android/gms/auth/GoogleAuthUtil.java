package com.google.android.gms.auth;

import android.accounts.AccountManager;
import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.ComponentName;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.res.Resources;
import android.os.Build.VERSION;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;
import com.google.android.gms.R;
import com.google.android.gms.common.GooglePlayServicesNotAvailableException;
import com.google.android.gms.common.GooglePlayServicesRepairableException;
import com.google.android.gms.common.GooglePlayServicesUtil;
import com.google.android.gms.common.a;
import com.google.android.gms.internal.do;
import com.google.android.gms.internal.er;
import com.google.android.gms.internal.s;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.io.IOException;
import java.net.URISyntaxException;

public final class GoogleAuthUtil {
    public static final String GOOGLE_ACCOUNT_TYPE = "com.google";
    public static final String KEY_ANDROID_PACKAGE_NAME;
    public static final String KEY_CALLER_UID;
    public static final String KEY_REQUEST_ACTIONS = "request_visible_actions";
    @Deprecated
    public static final String KEY_REQUEST_VISIBLE_ACTIVITIES = "request_visible_actions";
    public static final String KEY_SUPPRESS_PROGRESS_SCREEN = "suppressProgressScreen";
    private static final ComponentName vo;
    private static final ComponentName vp;
    private static final Intent vq;
    private static final Intent vr;

    static {
        KEY_CALLER_UID = VERSION.SDK_INT >= 11 ? "callerUid" : "callerUid";
        KEY_ANDROID_PACKAGE_NAME = VERSION.SDK_INT >= 14 ? "androidPackageName" : "androidPackageName";
        vo = new ComponentName(GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_PACKAGE, "com.google.android.gms.auth.GetToken");
        vp = new ComponentName(GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_PACKAGE, "com.google.android.gms.recovery.RecoveryService");
        vq = new Intent().setPackage(GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_PACKAGE).setComponent(vo);
        vr = new Intent().setPackage(GooglePlayServicesUtil.GOOGLE_PLAY_SERVICES_PACKAGE).setComponent(vp);
    }

    private GoogleAuthUtil() {
    }

    private static boolean K(String str) {
        return "NetworkError".equals(str) || "ServiceUnavailable".equals(str) || "Timeout".equals(str);
    }

    private static boolean L(String str) {
        return "BadAuthentication".equals(str) || "CaptchaRequired".equals(str) || "DeviceManagementRequiredOrSyncDisabled".equals(str) || "NeedPermission".equals(str) || "NeedsBrowser".equals(str) || "UserCancel".equals(str) || "AppDownloadRequired".equals(str);
    }

    private static String a(Context context, String str, String str2, Bundle bundle) throws IOException, UserRecoverableNotifiedException, GoogleAuthException {
        if (bundle == null) {
            bundle = new Bundle();
        }
        try {
            return getToken(context, str, str2, bundle);
        } catch (GooglePlayServicesAvailabilityException e) {
            int i;
            PendingIntent errorPendingIntent = GooglePlayServicesUtil.getErrorPendingIntent(e.getConnectionStatusCode(), context, 0);
            Resources resources = context.getResources();
            Notification notification = new Notification(17301642, resources.getString(R.string.auth_client_play_services_err_notification_msg), System.currentTimeMillis());
            notification.flags |= 16;
            CharSequence charSequence = context.getApplicationInfo().name;
            if (TextUtils.isEmpty(charSequence)) {
                ApplicationInfo applicationInfo;
                charSequence = context.getPackageName();
                PackageManager packageManager = context.getApplicationContext().getPackageManager();
                try {
                    applicationInfo = packageManager.getApplicationInfo(context.getPackageName(), 0);
                } catch (NameNotFoundException e2) {
                    applicationInfo = null;
                }
                if (applicationInfo != null) {
                    charSequence = packageManager.getApplicationLabel(applicationInfo).toString();
                }
            }
            CharSequence string = resources.getString(R.string.auth_client_requested_by_msg, new Object[]{charSequence});
            switch (e.getConnectionStatusCode()) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    i = R.string.auth_client_needs_installation_title;
                    break;
                case DetectedActivity.ON_FOOT /*2*/:
                    i = R.string.auth_client_needs_update_title;
                    break;
                case DetectedActivity.STILL /*3*/:
                    i = R.string.auth_client_needs_enabling_title;
                    break;
                default:
                    i = R.string.auth_client_using_bad_version_title;
                    break;
            }
            notification.setLatestEventInfo(context, resources.getString(i), string, errorPendingIntent);
            ((NotificationManager) context.getSystemService("notification")).notify(39789, notification);
            throw new UserRecoverableNotifiedException("User intervention required. Notification has been pushed.");
        } catch (UserRecoverableAuthException e3) {
            throw new UserRecoverableNotifiedException("User intervention required. Notification has been pushed.");
        }
    }

    private static void b(Intent intent) {
        if (intent == null) {
            throw new IllegalArgumentException("Callack cannot be null.");
        }
        try {
            Intent.parseUri(intent.toUri(1), 1);
        } catch (URISyntaxException e) {
            throw new IllegalArgumentException("Parameter callback contains invalid data. It must be serializable using toUri() and parseUri().");
        }
    }

    public static void clearToken(Context context, String token) throws GooglePlayServicesAvailabilityException, GoogleAuthException, IOException {
        Context applicationContext = context.getApplicationContext();
        er.ad("Calling this from your main thread can lead to deadlock");
        s(applicationContext);
        Bundle bundle = new Bundle();
        String str = context.getApplicationInfo().packageName;
        bundle.putString("clientPackageName", str);
        if (!bundle.containsKey(KEY_ANDROID_PACKAGE_NAME)) {
            bundle.putString(KEY_ANDROID_PACKAGE_NAME, str);
        }
        ServiceConnection aVar = new a();
        if (applicationContext.bindService(vq, aVar, 1)) {
            try {
                bundle = s.a.a(aVar.dm()).a(token, bundle);
                String string = bundle.getString(do.wf);
                if (bundle.getBoolean("booleanResult")) {
                    applicationContext.unbindService(aVar);
                    return;
                }
                throw new GoogleAuthException(string);
            } catch (Throwable e) {
                Log.i("GoogleAuthUtil", "GMS remote exception ", e);
                throw new IOException("remote exception");
            } catch (InterruptedException e2) {
                throw new GoogleAuthException("Interrupted");
            } catch (Throwable th) {
                applicationContext.unbindService(aVar);
            }
        } else {
            throw new IOException("Could not bind to service with the given context.");
        }
    }

    public static String getToken(Context context, String accountName, String scope) throws IOException, UserRecoverableAuthException, GoogleAuthException {
        return getToken(context, accountName, scope, new Bundle());
    }

    public static String getToken(Context context, String accountName, String scope, Bundle extras) throws IOException, UserRecoverableAuthException, GoogleAuthException {
        Context applicationContext = context.getApplicationContext();
        er.ad("Calling this from your main thread can lead to deadlock");
        s(applicationContext);
        extras = extras == null ? new Bundle() : new Bundle(extras);
        String str = context.getApplicationInfo().packageName;
        extras.putString("clientPackageName", str);
        if (!extras.containsKey(KEY_ANDROID_PACKAGE_NAME)) {
            extras.putString(KEY_ANDROID_PACKAGE_NAME, str);
        }
        ServiceConnection aVar = new a();
        if (applicationContext.bindService(vq, aVar, 1)) {
            try {
                Bundle a = s.a.a(aVar.dm()).a(accountName, scope, extras);
                Object string = a.getString("authtoken");
                if (TextUtils.isEmpty(string)) {
                    String string2 = a.getString("Error");
                    Intent intent = (Intent) a.getParcelable("userRecoveryIntent");
                    if (L(string2)) {
                        throw new UserRecoverableAuthException(string2, intent);
                    } else if (K(string2)) {
                        throw new IOException(string2);
                    } else {
                        throw new GoogleAuthException(string2);
                    }
                }
                applicationContext.unbindService(aVar);
                return string;
            } catch (Throwable e) {
                Log.i("GoogleAuthUtil", "GMS remote exception ", e);
                throw new IOException("remote exception");
            } catch (InterruptedException e2) {
                throw new GoogleAuthException("Interrupted");
            } catch (Throwable th) {
                applicationContext.unbindService(aVar);
            }
        } else {
            throw new IOException("Could not bind to service with the given context.");
        }
    }

    public static String getTokenWithNotification(Context context, String accountName, String scope, Bundle extras) throws IOException, UserRecoverableNotifiedException, GoogleAuthException {
        if (extras == null) {
            extras = new Bundle();
        }
        extras.putBoolean("handle_notification", true);
        return a(context, accountName, scope, extras);
    }

    public static String getTokenWithNotification(Context context, String accountName, String scope, Bundle extras, Intent callback) throws IOException, UserRecoverableNotifiedException, GoogleAuthException {
        b(callback);
        if (extras == null) {
            extras = new Bundle();
        }
        extras.putParcelable("callback_intent", callback);
        extras.putBoolean("handle_notification", true);
        return a(context, accountName, scope, extras);
    }

    public static String getTokenWithNotification(Context context, String accountName, String scope, Bundle extras, String authority, Bundle syncBundle) throws IOException, UserRecoverableNotifiedException, GoogleAuthException {
        if (TextUtils.isEmpty(authority)) {
            throw new IllegalArgumentException("Authority cannot be empty or null.");
        }
        if (extras == null) {
            extras = new Bundle();
        }
        if (syncBundle == null) {
            syncBundle = new Bundle();
        }
        ContentResolver.validateSyncExtrasBundle(syncBundle);
        extras.putString("authority", authority);
        extras.putBundle("sync_extras", syncBundle);
        extras.putBoolean("handle_notification", true);
        return a(context, accountName, scope, extras);
    }

    @Deprecated
    public static void invalidateToken(Context context, String token) {
        AccountManager.get(context).invalidateAuthToken(GOOGLE_ACCOUNT_TYPE, token);
    }

    private static void s(Context context) throws GooglePlayServicesAvailabilityException, GoogleAuthException {
        try {
            GooglePlayServicesUtil.s(context);
        } catch (GooglePlayServicesRepairableException e) {
            throw new GooglePlayServicesAvailabilityException(e.getConnectionStatusCode(), e.getMessage(), e.getIntent());
        } catch (GooglePlayServicesNotAvailableException e2) {
            throw new GoogleAuthException(e2.getMessage());
        }
    }
}
