package com.alcinoe.launchscreen;

import android.os.Bundle;
import android.content.Intent;
import androidx.appcompat.app.AppCompatActivity;

/** 
 * To have a custom launchscreen, in AndroidManifest.template.xml you need 
 * to add this activity :
 *
 * <activity android:name="com.alcinoe.launchscreen.ALLaunchScreenActivity"
 *           android:theme="@style/MyLaunchTheme">
 *   <intent-filter>  
 *       <action android:name="android.intent.action.MAIN"/>
 *       <category android:name="android.intent.category.LAUNCHER"/>
 *   </intent-filter> 
 * </activity>
 *
 * and define in styles.xml an entry like :
 *
 * <style name="MyLaunchTheme" parent="@style/MyTheme"> 
 *   <item name="android:windowDrawsSystemBarBackgrounds">true</item>
 *   <item name="android:statusBarColor">@color/my_statusbar_color</item>
 *   <item name="android:background">@drawable/launchscreen_background</item>
 * </style> 
*/
public class ALLaunchScreenActivity extends AppCompatActivity {
    
  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    startActivity(new Intent(this, com.embarcadero.firemonkey.FMXNativeActivity.class));
    finish();
  }
  
}