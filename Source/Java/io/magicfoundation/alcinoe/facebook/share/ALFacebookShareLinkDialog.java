package io.magicfoundation.alcinoe.facebook.share;

import com.facebook.share.widget.ShareDialog;
import com.facebook.share.model.ShareLinkContent;
import android.app.Activity;
import android.net.Uri;

public class ALFacebookShareLinkDialog {

  public static boolean canShow() {
      return ShareDialog.canShow(ShareLinkContent.class);
  }
        
  public static void show(final Activity activity,
                          final Uri contentUrl,
                          final String quote) {

    ShareLinkContent linkContent = new ShareLinkContent.Builder()    
      .setContentUrl(contentUrl)
      .setQuote(quote)
      .build();         
    ShareDialog.show(activity, linkContent);
    
  }
        
}
