package com.alcinoe.location;

import android.content.Context;
import android.location.Location;
import android.util.Log;
import android.support.annotation.NonNull;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationCallback;
import com.google.android.gms.location.LocationResult;

public class ALLocationServices {

  private static final String TAG = "ALLocationServices";
  private FusedLocationProviderClient mFusedLocationClient;
  private ALLocationServicesListener mLocationServicesListener;
  private Context mContext;
  	
  //https://developers.google.com/android/reference/com/google/android/gms/location/LocationCallback
  //Used for receiving notifications from the FusedLocationProviderApi when the device location 
  //has changed or can no longer be determined. The methods are called if the LocationCallback 
  //has been registered with the location client using the requestLocationUpdates(GoogleApiClient, 
  //LocationRequest, LocationCallback, Looper) method.
  private LocationCallback mLocationCallback = new LocationCallback(){
      
      //Called when device location information is available.
      //The most recent location returned by locationResult.getLastLocation() is not guaranteed 
      //to be immediately fresh, but will be reasonably up to date given the 
      //hints specified by the active LocationRequests.
      @Override
      public void onLocationResult(LocationResult locationResult) {
        
          //if no mLocationServicesListener exit
          if (mLocationServicesListener == null) { return; }
          
          //Returns locations computed, ordered from oldest to newest.
          //No duplicate locations will be returned to any given listener 
          //(i.e. locations will not overlap in time between subsequent calls to a listener).
          for (Location location : locationResult.getLocations()) {
            mLocationServicesListener.onLocationChanged(location);           
          }
          
      };

  };
        
  public ALLocationServices(final Context context){
  
    //init local members
    this.mContext = context;
    this.mLocationServicesListener = null; 
    
    //Create a new instance of FusedLocationProviderClient for use in a non-activity Context. 
    //Error resolutions will be automatically launched from the provided Context, displaying system tray notifications when necessary.
    this.mFusedLocationClient = LocationServices.getFusedLocationProviderClient(context);
  
  }   

  public void setListener(ALLocationServicesListener listener) {
    mLocationServicesListener = listener;
  }    
    
  public void startLocationUpdates(boolean startWithLastKnownLocation, 
                                   long interval, 
                                   long fastestInterval, 
                                   long maxWaitTime, 
                                   int priority, 
                                   float smallestDisplacement) {
    
    if (startWithLastKnownLocation) {     
     
      //create a tmp FusedLocationProviderClient
      FusedLocationProviderClient tmpFusedLocationClient = LocationServices.getFusedLocationProviderClient(mContext);
     
      //Returns the best most recent location currently available.
      //If a location is not available, which should happen very rarely, null will be returned. The best accuracy 
      //available while respecting the location permissions will be returned.
      //This method provides a simplified way to get location. It is particularly well suited for applications 
      //that do not require an accurate location and that do not want to maintain extra logic for location updates.
      tmpFusedLocationClient.getLastLocation()
        .addOnSuccessListener(new OnSuccessListener<Location>() {
            @Override
            public void onSuccess(Location location) {
                if (location == null) { 
                  // https://stackoverflow.com/questions/49599102/fusedlocationapi-getlastlocation-and-requestlocationupdates-take-very-long-time
                  // the problem is that the following requestLocationUpdates can take a very long time to return according to mLocationRequest
                  // so maybe we must for the very first update change the parameter of mLocationRequest to retrieve the very first Location
                  // as fast as possible ?
                  Log.w(TAG, "Their is no location currently available"); 
                  return;
                }
                if (mLocationServicesListener != null) { mLocationServicesListener.onLocationChanged(location); }
            }
        })
        .addOnFailureListener(new OnFailureListener() {
            @Override
            public void onFailure(@NonNull Exception e) {
                Log.w(TAG, "Error trying to get last location");
            }
        });     
    
    }
        
    // https://developers.google.com/android/reference/com/google/android/gms/location/LocationRequest
    LocationRequest mLocationRequest = new LocationRequest();
    mLocationRequest.setInterval(interval);
    mLocationRequest.setFastestInterval(fastestInterval);
    mLocationRequest.setMaxWaitTime(maxWaitTime);
    mLocationRequest.setPriority(priority);
    mLocationRequest.setSmallestDisplacement(smallestDisplacement);        
    
    //https://developers.google.com/android/reference/com/google/android/gms/location/FusedLocationProviderClient.html#getLastLocation()
    //Requests location updates with a callback on the specified Looper thread.
    //Any previous LocationRequests registered on this LocationListener will be replaced.
    //This call will keep the Google Play services connection active, so make sure to call 
    //removeLocationUpdates(LocationCallback) when you no longer need it, otherwise you lose 
    //the benefits of the automatic connection management.
    // * request	The location request for the updates.
    // * looper: The Looper object whose message queue will be used to implement the callback 
    // * callback	The callback for the location updates.
    //   mechanism, or null to make callbacks on the calling thread.
    // * Returns: a Task for the call, check isSuccessful() to determine if it was successful.
    mFusedLocationClient.requestLocationUpdates(mLocationRequest, mLocationCallback, null);
    
  }

  public void stopLocationUpdates() {
    
    //Removes all location updates for the given location result listener.
    // * callback	The callback to remove.
    // * Returns: a Task for the call, check isSuccessful() to determine if it was successful.
    mFusedLocationClient.removeLocationUpdates(mLocationCallback);
    
  }  
 
}