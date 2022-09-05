package com.alcinoe.appsflyer;

import java.util.HashMap;
import android.app.Application;
import android.app.Activity;
import android.content.Context;
import com.appsflyer.AppsFlyerLib;
import com.appsflyer.AppsFlyerConversionListener;

public class ALAppsFlyer {
  
  /* https://stackoverflow.com/questions/53141813/delphi-android-java-class-init-function */
  public static void initialize(String key, AppsFlyerConversionListener conversionListener, Context context) {
    AppsFlyerLib.getInstance().init(key, conversionListener, context);
  }
  
  public static void sendDeepLinkData(Activity activity) {
    AppsFlyerLib.getInstance().sendDeepLinkData(activity);
  }
  
  public static void startTracking(Application application) {
    AppsFlyerLib.getInstance().startTracking(application);
  }
  
  public static void trackEvent(Context context, String eventName, HashMap<String, Object> eventValues) {
    AppsFlyerLib.getInstance().trackEvent(context, eventName, eventValues);
  }
  
  public static void trackEvent(Context context, String eventName) {
    AppsFlyerLib.getInstance().trackEvent(context, eventName, null);
  }
  
  public static void unregisterConversionListener() {
    AppsFlyerLib.getInstance().unregisterConversionListener();
  }
  
  public static void setAndroidIdData(String androidIdData) {
    AppsFlyerLib.getInstance().setAndroidIdData(androidIdData);
  }

  public static void enableUninstallTracking(String senderId) {
    AppsFlyerLib.getInstance().enableUninstallTracking(senderId);
  }
  
  public static void updateServerUninstallToken(Context context, String refreshedToken) {
    AppsFlyerLib.getInstance().updateServerUninstallToken(context, refreshedToken);
  }
  
  public static void setCustomerUserId(String id) {
    AppsFlyerLib.getInstance().setCustomerUserId(id);
  }

}