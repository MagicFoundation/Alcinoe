package com.alcinoe.facebook;

import com.facebook.share.widget.ShareDialog;
import com.facebook.share.model.ShareLinkContent;
import android.app.Activity;
import android.net.Uri;

public class ALFacebookShareLinkDialog {

  public static boolean canShow() {
      return ShareDialog.canShow(ShareLinkContent.class);
  }
        
  /**
   * @param contentDescription The contentDescription of the link.
   * @param contentTitle The link contentTitle.
   * @param imageUrl The network URL of an image.
   * @param quote The text quoted from the link.
   */
  public static void show(final Activity activity,
                          final Uri contentUrl,
                          final String contentTitle,
                          final String contentDescription,
                          final Uri imageUrl,
                          final String quote) {

    ShareLinkContent linkContent = new ShareLinkContent.Builder()    
      .setContentUrl(contentUrl)
      .setContentTitle(contentTitle)
      .setContentDescription(contentDescription)
      .setImageUrl(imageUrl)
      .setQuote(quote)
      .build();         
    ShareDialog.show(activity, linkContent);
    
  }
        
}
