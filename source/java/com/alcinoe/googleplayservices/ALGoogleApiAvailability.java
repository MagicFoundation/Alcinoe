package com.alcinoe.googleplayservices;

import com.google.android.gms.common.GoogleApiAvailability; 
import android.app.Activity;

public class ALGoogleApiAvailability {
  
  public static int isGooglePlayServicesAvailable(Activity activity) {
    GoogleApiAvailability googleApiAvailability = GoogleApiAvailability.getInstance();
    return googleApiAvailability.isGooglePlayServicesAvailable(activity);
  }
      
}
