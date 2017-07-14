package com.alcinoe.location;

import android.content.Context;
import android.location.Location;
import android.os.Bundle;
import android.util.Log;
import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.GoogleApiClient.ConnectionCallbacks;
import com.google.android.gms.common.api.GoogleApiClient.OnConnectionFailedListener;
import com.google.android.gms.location.LocationListener;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.common.GoogleApiAvailability;

public class ALLocationServices implements GoogleApiClient.ConnectionCallbacks, GoogleApiClient.OnConnectionFailedListener, LocationListener {

  private GoogleApiClient mGoogleApiClient; 
  private ALLocationServicesListener mLocationServicesListener;
  private LocationRequest mLocationRequest;
  private boolean mStartWithLastKnownLocation;
        
  public ALLocationServices(final Context context){
    this.mLocationServicesListener = null;
    this.mLocationRequest = null;  
    this.mStartWithLastKnownLocation = false;
    if (GoogleApiAvailability.getInstance().isGooglePlayServicesAvailable(context) == ConnectionResult.SUCCESS) {
      this.mGoogleApiClient = new GoogleApiClient.Builder(context)
        .addApi(LocationServices.API)
        .addConnectionCallbacks(this)
        .addOnConnectionFailedListener(this)
        .build();
    }
    else {
      Log.w("ALLocationServices", "Google Play Services is unavailable");
      this.mGoogleApiClient = null;
    }
  }   

  public void setListener(ALLocationServicesListener listener) {
    mLocationServicesListener = listener;
  }    
  
  private void doStartLocationUpdates() {        
    if (mStartWithLastKnownLocation) {
      Location location = LocationServices.FusedLocationApi.getLastLocation(mGoogleApiClient); 
      onLocationChanged(location);
    }
    LocationServices.FusedLocationApi.requestLocationUpdates(mGoogleApiClient, mLocationRequest, this);
  }
  
  // https://developers.google.com/android/reference/com/google/android/gms/location/LocationRequest
  public void startLocationUpdates(boolean startWithLastKnownLocation, 
                                   long interval, 
                                   long fastestInterval, 
                                   long maxWaitTime, 
                                   int priority, 
                                   float smallestDisplacement) {
    
    mStartWithLastKnownLocation = startWithLastKnownLocation;
    
    mLocationRequest = new LocationRequest();
    mLocationRequest.setInterval(interval);
    mLocationRequest.setFastestInterval(fastestInterval);
    mLocationRequest.setMaxWaitTime(maxWaitTime);
    mLocationRequest.setPriority(priority);
    mLocationRequest.setSmallestDisplacement(smallestDisplacement);
        
    if (mGoogleApiClient != null) { 
      mGoogleApiClient.connect();
    }
    
  }

  public void stopLocationUpdates() {
    if (mGoogleApiClient != null) { 
      if (mGoogleApiClient.isConnected()) { LocationServices.FusedLocationApi.removeLocationUpdates(mGoogleApiClient, this); }
      mGoogleApiClient.disconnect(); // If the connection to the remote service hasn't been established yet, all enqueued calls will be canceled.
    }
  }  
 
  @Override
  public void onConnected(Bundle connectionHint) {   
    Log.i("ALLocationServices", "GoogleApiClient connected");
    doStartLocationUpdates();
  }

  @Override
  public void onConnectionFailed(ConnectionResult result) {
    Log.e("ALLocationServices", "GoogleApiClient connection failed: " + result.toString());
    // i don't know what else i can do from here, as this object is used inside a service and start 
    // at device boot's up i guess it's bad idea to ask for resolution (service have no ui)
    // so silently skip this error
  }

  @Override
  public void onConnectionSuspended(int cause){
    Log.w("ALLocationServices", "GoogleApiClient connection suspended");
    //GoogleApiClient will automatically attempt to restore the connection. Applications should disable UI components 
    //that require the service, and wait for a call to onConnected(Bundle) to re-enable them.
  }
 
  @Override
  public void onLocationChanged(Location location) {
    if ((mLocationServicesListener != null) && 
        (location != null)) { mLocationServicesListener.onLocationChanged(location); }  
  } 

}