package com.google.android.gms.location;

import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.location.Location;
import android.os.Looper;
import com.google.android.gms.common.GooglePlayServicesClient;
import com.google.android.gms.common.GooglePlayServicesClient.ConnectionCallbacks;
import com.google.android.gms.common.GooglePlayServicesClient.OnConnectionFailedListener;
import com.google.android.gms.internal.er;
import com.google.android.gms.internal.hi;
import com.google.android.gms.internal.hj;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class LocationClient implements GooglePlayServicesClient {
    public static final String KEY_LOCATION_CHANGED = "com.google.android.location.LOCATION";
    public static final String KEY_MOCK_LOCATION = "mockLocation";
    private final hi KO;

    public interface OnAddGeofencesResultListener {
        void onAddGeofencesResult(int i, String[] strArr);
    }

    public interface OnRemoveGeofencesResultListener {
        void onRemoveGeofencesByPendingIntentResult(int i, PendingIntent pendingIntent);

        void onRemoveGeofencesByRequestIdsResult(int i, String[] strArr);
    }

    public LocationClient(Context context, ConnectionCallbacks connectionCallbacks, OnConnectionFailedListener connectionFailedListener) {
        this.KO = new hi(context, connectionCallbacks, connectionFailedListener, "location");
    }

    public static int getErrorCode(Intent intent) {
        return intent.getIntExtra("gms_error_code", -1);
    }

    public static int getGeofenceTransition(Intent intent) {
        int intExtra = intent.getIntExtra("com.google.android.location.intent.extra.transition", -1);
        if (intExtra == -1) {
            return -1;
        }
        return (intExtra == 1 || intExtra == 2 || intExtra == 4) ? intExtra : -1;
    }

    public static List<Geofence> getTriggeringGeofences(Intent intent) {
        ArrayList arrayList = (ArrayList) intent.getSerializableExtra("com.google.android.location.intent.extra.geofence_list");
        if (arrayList == null) {
            return null;
        }
        ArrayList arrayList2 = new ArrayList(arrayList.size());
        Iterator it = arrayList.iterator();
        while (it.hasNext()) {
            arrayList2.add(hj.h((byte[]) it.next()));
        }
        return arrayList2;
    }

    public static boolean hasError(Intent intent) {
        return intent.hasExtra("gms_error_code");
    }

    public void addGeofences(List<Geofence> geofences, PendingIntent pendingIntent, OnAddGeofencesResultListener listener) {
        List list = null;
        if (geofences != null) {
            List arrayList = new ArrayList();
            for (Geofence geofence : geofences) {
                er.b(geofence instanceof hj, (Object) "Geofence must be created using Geofence.Builder.");
                arrayList.add((hj) geofence);
            }
            list = arrayList;
        }
        this.KO.addGeofences(list, pendingIntent, listener);
    }

    public void connect() {
        this.KO.connect();
    }

    public void disconnect() {
        this.KO.disconnect();
    }

    public Location getLastLocation() {
        return this.KO.getLastLocation();
    }

    public boolean isConnected() {
        return this.KO.isConnected();
    }

    public boolean isConnecting() {
        return this.KO.isConnecting();
    }

    public boolean isConnectionCallbacksRegistered(ConnectionCallbacks listener) {
        return this.KO.isConnectionCallbacksRegistered(listener);
    }

    public boolean isConnectionFailedListenerRegistered(OnConnectionFailedListener listener) {
        return this.KO.isConnectionFailedListenerRegistered(listener);
    }

    public void registerConnectionCallbacks(ConnectionCallbacks listener) {
        this.KO.registerConnectionCallbacks(listener);
    }

    public void registerConnectionFailedListener(OnConnectionFailedListener listener) {
        this.KO.registerConnectionFailedListener(listener);
    }

    public void removeGeofences(PendingIntent pendingIntent, OnRemoveGeofencesResultListener listener) {
        this.KO.removeGeofences(pendingIntent, listener);
    }

    public void removeGeofences(List<String> geofenceRequestIds, OnRemoveGeofencesResultListener listener) {
        this.KO.removeGeofences((List) geofenceRequestIds, listener);
    }

    public void removeLocationUpdates(PendingIntent callbackIntent) {
        this.KO.removeLocationUpdates(callbackIntent);
    }

    public void removeLocationUpdates(LocationListener listener) {
        this.KO.removeLocationUpdates(listener);
    }

    public void requestLocationUpdates(LocationRequest request, PendingIntent callbackIntent) {
        this.KO.requestLocationUpdates(request, callbackIntent);
    }

    public void requestLocationUpdates(LocationRequest request, LocationListener listener) {
        this.KO.requestLocationUpdates(request, listener);
    }

    public void requestLocationUpdates(LocationRequest request, LocationListener listener, Looper looper) {
        this.KO.requestLocationUpdates(request, listener, looper);
    }

    public void setMockLocation(Location mockLocation) {
        this.KO.setMockLocation(mockLocation);
    }

    public void setMockMode(boolean isMockMode) {
        this.KO.setMockMode(isMockMode);
    }

    public void unregisterConnectionCallbacks(ConnectionCallbacks listener) {
        this.KO.unregisterConnectionCallbacks(listener);
    }

    public void unregisterConnectionFailedListener(OnConnectionFailedListener listener) {
        this.KO.unregisterConnectionFailedListener(listener);
    }
}
