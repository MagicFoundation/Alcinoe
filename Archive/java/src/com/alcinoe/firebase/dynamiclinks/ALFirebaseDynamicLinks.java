package com.alcinoe.firebase.dynamiclinks;

import android.app.Activity;
import android.net.Uri;
import android.support.annotation.NonNull;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.firebase.appinvite.FirebaseAppInvite;
import com.google.firebase.dynamiclinks.FirebaseDynamicLinks;
import com.google.firebase.dynamiclinks.PendingDynamicLinkData;
import com.alcinoe.firebase.dynamiclinks.ALFirebaseDynamicLinksListener;

public class ALFirebaseDynamicLinks {
 
  private Activity mActivity;
  private ALFirebaseDynamicLinksListener mListener;
  public static final int ERROR_TASK_FAILED = 1;
  public static final int ERROR_NO_PENDING_LINK = 2;
  
  public ALFirebaseDynamicLinks(final Activity activity){
    this.mActivity = activity;
    this.mListener = null;  
  } 
       
  public void setListener(ALFirebaseDynamicLinksListener listener) {
    this.mListener = listener;
  }    
    
  public void getDynamicLink() {
    
    FirebaseDynamicLinks.getInstance()
      .getDynamicLink(mActivity.getIntent())
      .addOnSuccessListener(mActivity, new OnSuccessListener<PendingDynamicLinkData>() {
        
          @Override
          public void onSuccess(PendingDynamicLinkData data) {
            
            //In the callback the PendingDynamicLinkData is returned in addOnSuccessListener(OnSuccessListener) 
            //or addOnCompleteListener(Activity, OnCompleteListener) which returns the most recently clicked 
            //dynamic link, or null if a dynamic link was not pending as captured data or in the intent.           
            if (data == null) {
              if (mListener != null) mListener.onGetDynamicLinkError(ERROR_NO_PENDING_LINK, "no pending dynamic link");
              return;
            }
            
            //Return the link parameter of the dynamic link.
            Uri deepLink = data.getLink();
            
            //Return FirebaseAppInvite instance or null if the dynamic link is not an invitation dynamic link.
            String invitationId = "";
            FirebaseAppInvite invite = FirebaseAppInvite.getInvitation(data);
            if (invite != null) { invitationId = invite.getInvitationId(); }
            
            //call the listener
            if (mListener != null) mListener.onGetDynamicLinkSuccess(deepLink.toString(), invitationId);
            
          }
      
      })
      .addOnFailureListener(mActivity, new OnFailureListener() {
      
          @Override
          public void onFailure(@NonNull Exception e) {
            if (mListener != null) mListener.onGetDynamicLinkError(ERROR_TASK_FAILED, e.getMessage());
          }
      
      });

  }      
          
}