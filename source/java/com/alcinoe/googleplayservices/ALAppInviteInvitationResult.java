package com.alcinoe.googleplayservices;

import com.alcinoe.googleplayservices.ALAppInviteInvitationResultListener;
import com.google.android.gms.appinvite.AppInvite;
import com.google.android.gms.appinvite.AppInviteReferral;
import com.google.android.gms.appinvite.AppInviteInvitationResult;
import com.google.android.gms.common.ConnectionResult; 
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.ResultCallback; 
import android.util.Log;
import android.os.Bundle;
import android.content.Intent;
import android.app.Activity;

public class ALAppInviteInvitationResult implements GoogleApiClient.ConnectionCallbacks, GoogleApiClient.OnConnectionFailedListener {
 
  private Activity mActivity;
  private GoogleApiClient mGoogleApiClient; 
  private boolean mAutoLaunchDeepLink;
  private ALAppInviteInvitationResultListener mAppInviteInvitationResultListener;
  
  public ALAppInviteInvitationResult(final Activity activity){
    this.mActivity = activity;
    this.mGoogleApiClient = null;
    this.mAutoLaunchDeepLink = false;
    this.mAppInviteInvitationResultListener = null;  
  } 
       
  public void setListener(ALAppInviteInvitationResultListener listener) {
    this.mAppInviteInvitationResultListener = listener;
  }    
    
  public void retrieve(final boolean autoLaunchDeepLink) {
    this.mAutoLaunchDeepLink = autoLaunchDeepLink;
    this.mGoogleApiClient = new GoogleApiClient.Builder(this.mActivity)
      .addApi(AppInvite.API)
      .addConnectionCallbacks(this)
      .addOnConnectionFailedListener(this)
      .build();
    this.mGoogleApiClient.connect();
  }      
    
  @Override
  public void onConnected(Bundle connectionHint) {
    
    final ALAppInviteInvitationResult InvitationResult = this;
    
    AppInvite.AppInviteApi.getInvitation(mGoogleApiClient, mActivity, mAutoLaunchDeepLink)
      .setResultCallback(
        new ResultCallback<AppInviteInvitationResult>() {
         
          @Override
          public void onResult(AppInviteInvitationResult result) {
     
            if (result.getStatus().isSuccess()) {                
              Intent intent = result.getInvitationIntent();
              String deepLink = AppInviteReferral.getDeepLink(intent);
              String invitationId = AppInviteReferral.getInvitationId(intent);     
              if (mAppInviteInvitationResultListener != null) mAppInviteInvitationResultListener.onSuccess(deepLink, invitationId); 
            }
            else {              
              if (mAppInviteInvitationResultListener != null) mAppInviteInvitationResultListener.onError(2, 0);  
            }
            
            mGoogleApiClient.unregisterConnectionCallbacks(InvitationResult);
            mGoogleApiClient.unregisterConnectionFailedListener(InvitationResult);
            mGoogleApiClient.disconnect();
                   
          }
        
        });

  }
  
  @Override
  public void onConnectionFailed(ConnectionResult result) {
    if (this.mAppInviteInvitationResultListener != null) this.mAppInviteInvitationResultListener.onError(1, result.getErrorCode());
    mGoogleApiClient.unregisterConnectionCallbacks(this);
    mGoogleApiClient.unregisterConnectionFailedListener(this);
  }

  @Override
  public void onConnectionSuspended(int cause){
    //GoogleApiClient will automatically attempt to restore the connection. Applications should disable UI components 
    //that require the service, and wait for a call to onConnected(Bundle) to re-enable them.
  }
      
}
