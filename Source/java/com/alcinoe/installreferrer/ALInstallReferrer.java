package com.alcinoe.installreferrer;

import android.content.Context;
import android.util.Log;
import android.os.Handler;
import android.os.Looper;
import com.android.installreferrer.api.InstallReferrerClient;
import com.android.installreferrer.api.InstallReferrerStateListener;
import com.android.installreferrer.api.InstallReferrerClient.InstallReferrerResponse;
import com.android.installreferrer.api.ReferrerDetails;

public class ALInstallReferrer {
   
  private static final String TAG = "ALInstallReferrer";
  private ALInstallReferrerListener mListener;
  private Context mContext;
  private int mRetries;
	
  public ALInstallReferrer(Context context) {
    super(); 
    this.mContext = context;
    this.mListener = null;
    this.mRetries = 0;
  }    
  
  public void setListener(ALInstallReferrerListener listener) {
    mListener = listener;
  }    

  public void getInstallReferrer() {
      
    InstallReferrerClient referrerClient = InstallReferrerClient.newBuilder(mContext).build();
    
    try {

      referrerClient.startConnection(new InstallReferrerStateListener() {

        @Override
        public void onInstallReferrerSetupFinished(int responseCode) {
        
          boolean retryAtEnd = false;
          switch (responseCode) {
     
            /** Success. */
            case InstallReferrerResponse.OK:
                
                ReferrerDetails referrerDetails;
                try {
                  
                   referrerDetails = referrerClient.getInstallReferrer(); // This method uses information stored by the Google Play Store app without initiating a network request.
                
                } 
                catch (Throwable e) {
                  referrerDetails = null;
                }

                if (referrerDetails != null) {                   
                  Log.v(TAG, "Install Referrer read successfully. Closing connection");
                  if (mListener != null) { 
                    mListener.onGetInstallReferrerSuccess(referrerDetails.getInstallReferrer(), 
                                                          referrerDetails.getReferrerClickTimestampSeconds(), 
                                                          referrerDetails.getInstallBeginTimestampSeconds()); 
                  }
                }
                else {
                  Log.w(TAG, "Couldn't get install referrer from client. Retrying...");
                  retryAtEnd = true;
                }
                
                break;
   
   
            /** Install Referrer API not supported by the installed Play Store app. */
            case InstallReferrerResponse.FEATURE_NOT_SUPPORTED:
                Log.e(TAG, "Install Referrer API not supported by the installed Play Store app. Closing connection");
                if (mListener != null) { 
                  mListener.onGetInstallReferrerError(responseCode); 
                }
                break;
   
   
            /** Could not initiate connection to the Install Referrer service. */
            case InstallReferrerResponse.SERVICE_UNAVAILABLE:
                Log.w(TAG, "Could not initiate connection to the Install Referrer service. Retrying...");
                retryAtEnd = true;
                break;
   
   
            /**
             * Play Store service is not connected now - potentially transient state.
             *
             * <p>E.g. Play Store could have been updated in the background while your app was still
             * running. So feel free to introduce your retry policy for such use case. It should lead to a
             * call to {@link #startConnection(InstallReferrerStateListener)} right after or in some time
             * after you received this code.
             */
            case InstallReferrerResponse.SERVICE_DISCONNECTED:
                Log.w(TAG, "Play Store service is not connected now. Retrying...");
                retryAtEnd = true;
                break;
   
   
            /** General errors caused by incorrect usage */
            case InstallReferrerResponse.DEVELOPER_ERROR:
                Log.w(TAG, "Install Referrer API general errors caused by incorrect usage. Retrying...");
                retryAtEnd = true;
                break;
                
   
            /** default */
            default:
                Log.e(TAG, "Unexpected response code of install referrer response. Closing connection");
                if (mListener != null) { 
                  mListener.onGetInstallReferrerError(responseCode); 
                }
                break;
     
          }
     
          referrerClient.endConnection();
          
          if (retryAtEnd) { 
            
            if (mRetries + 1 > 20) {
                Log.e(TAG, "Limit number of retry for install referrer surpassed");
                if (mListener != null) { 
                  mListener.onGetInstallReferrerError(responseCode); 
                }
                return;
            }

            mRetries++;

            final Handler handler = new Handler(Looper.getMainLooper());
            handler.postDelayed(new Runnable() {
              @Override
              public void run() {
                getInstallReferrer();
              }
            }, 3000);
            
          }

        }

        @Override
        public void onInstallReferrerServiceDisconnected() {
          // Called to notify that connection to install referrer service was lost.
          // Note: This does not remove install referrer service connection itself - this 
          // binding to the service will remain active, and you will receive a call to 
          // onInstallReferrerSetupFinished(int) when install referrer service is 
          // next running and setup is complete.
        }

      });  

    } 
    catch (Throwable e) {
      //https://stackoverflow.com/questions/60463175/installreferrerclient-startconnection-throwing-caused-by-java-lang-securityexc
      //https://issuetracker.google.com/issues/72926755
      Log.e(TAG, "startConnection - Exception", e); 
      if (mListener != null) { 
        mListener.onGetInstallReferrerError(InstallReferrerResponse.FEATURE_NOT_SUPPORTED); 
      }
    }

  }
       
}