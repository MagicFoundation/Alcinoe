package com.alcinoe.facebook;

import com.facebook.applinks.AppLinkData;
import android.app.Activity;
import android.net.Uri;

public class ALFacebookDeferredAppLinkDataResult {

  private Activity mActivity;
  private ALFacebookDeferredAppLinkDataResultListener mFaceBookDeferredAppLinkDataResultListener;
  
  public ALFacebookDeferredAppLinkDataResult(final Activity activity){
    this.mActivity = activity;
    this.mFaceBookDeferredAppLinkDataResultListener = null;
  } 

  public void setListener(ALFacebookDeferredAppLinkDataResultListener listener) {
    this.mFaceBookDeferredAppLinkDataResultListener = listener;
  }    

  public void retrieve() {
    
    final ALFacebookDeferredAppLinkDataResult facebookDeferredAppLinkDataResult = this;
    
    AppLinkData.fetchDeferredAppLinkData(mActivity, 
      new AppLinkData.CompletionHandler() {
        
        @Override
        public void onDeferredAppLinkDataFetched(AppLinkData appLinkData) {
          if (appLinkData != null) {
            Uri targetUri = appLinkData.getTargetUri(); 
            String promotionCode = appLinkData.getPromotionCode(); 
            if (facebookDeferredAppLinkDataResult.mFaceBookDeferredAppLinkDataResultListener != null) facebookDeferredAppLinkDataResult.mFaceBookDeferredAppLinkDataResultListener.onSuccess(targetUri.toString(), promotionCode); 
          }
          else {
            if (facebookDeferredAppLinkDataResult.mFaceBookDeferredAppLinkDataResultListener != null) facebookDeferredAppLinkDataResult.mFaceBookDeferredAppLinkDataResultListener.onError(0);  
          }
        }

     }
    );
  }  

} 