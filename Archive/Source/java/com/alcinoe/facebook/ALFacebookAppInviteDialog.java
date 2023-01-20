package com.alcinoe.facebook;

import com.facebook.share.widget.AppInviteDialog;
import com.facebook.share.model.AppInviteContent;
import android.app.Activity;

public class ALFacebookAppInviteDialog {

  public static boolean canShow() {
      return AppInviteDialog.canShow();
      
  }
        
  /**
   * @param applinkUrl the applink url
   * @param promotionText Promotion text to be shown on sender and receiver flows.
   *                      Promotion text has to be between 1 and 80 characters long.
   * @param promotionCode Promotion code to be shown on sender and receiver flows.
   *                      Promotion code is optional and has to be less than 10 characters
   *                      long. promotionText needs to be specified if promotionCode
   *                      is provided.
   * @param previewImageUrl url of the image that is going to be used as a preview for invite
   * @return the builder
   */
  public static void show(final Activity activity,
                          final String applinkUrl,
                          final String promotionText,
                          final String promotionCode,
                          final String previewImageUrl) {

    AppInviteContent content = new AppInviteContent.Builder()
      .setApplinkUrl(applinkUrl)
      .setPromotionDetails(promotionText, promotionCode)
      .setPreviewImageUrl(previewImageUrl)
      .build();
    AppInviteDialog.show(activity, content);

  }
        
}
