package com.alcinoe.firebase.crash;

import com.google.firebase.crash.FirebaseCrash;

public class ALFirebaseCrash {

  public static void report(final String message) {
    
    FirebaseCrash.report(new Exception(message));  
  
  }
       
}