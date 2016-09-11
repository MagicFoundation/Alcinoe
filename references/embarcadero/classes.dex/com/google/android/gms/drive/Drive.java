package com.google.android.gms.drive;

import android.content.Context;
import android.os.Looper;
import com.google.android.gms.common.Scopes;
import com.google.android.gms.common.api.Api;
import com.google.android.gms.common.api.Api.a;
import com.google.android.gms.common.api.Api.b;
import com.google.android.gms.common.api.GoogleApiClient.ApiOptions;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.common.api.Scope;
import com.google.android.gms.drive.internal.l;
import com.google.android.gms.drive.internal.n;
import com.google.android.gms.drive.internal.p;
import com.google.android.gms.internal.ee;
import java.util.List;

public final class Drive {
    public static final Api API;
    public static final Scope Da;
    public static final c Db;
    public static final DriveApi DriveApi;
    public static final Scope SCOPE_APPFOLDER;
    public static final Scope SCOPE_FILE;
    public static final b<n> va;

    static {
        va = new b<n>() {
            public /* synthetic */ a b(Context context, Looper looper, ee eeVar, ApiOptions apiOptions, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
                return d(context, looper, eeVar, apiOptions, connectionCallbacks, onConnectionFailedListener);
            }

            public n d(Context context, Looper looper, ee eeVar, ApiOptions apiOptions, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener onConnectionFailedListener) {
                List dT = eeVar.dT();
                return new n(context, looper, eeVar, connectionCallbacks, onConnectionFailedListener, (String[]) dT.toArray(new String[dT.size()]));
            }

            public int getPriority() {
                return Integer.MAX_VALUE;
            }
        };
        SCOPE_FILE = new Scope(Scopes.DRIVE_FILE);
        SCOPE_APPFOLDER = new Scope(Scopes.DRIVE_APPFOLDER);
        Da = new Scope(Scopes.DRIVE_FULL);
        API = new Api(va, new Scope[0]);
        DriveApi = new l();
        Db = new p();
    }

    private Drive() {
    }
}
